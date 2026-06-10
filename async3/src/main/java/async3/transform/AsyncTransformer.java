package async3.transform;

import org.objectweb.asm.ClassReader;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;
import org.objectweb.asm.tree.analysis.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.objectweb.asm.Opcodes.*;

/**
 * Phase 1 of the async3 prototype: rewrites methods containing calls to the
 * {@code AsyncRT.await} marker into resumable state machines, at the bytecode level.
 *
 * <p>For each such method {@code m} this produces:
 * <ul>
 *   <li>a state machine class {@code Owner$async$m$i extends FutureStateMachine} whose
 *       {@code apply(Object tr)} is the transformed copy of {@code m}'s body;</li>
 *   <li>a sibling entry point {@code m$async(args): CompletableFuture} on the original class;</li>
 *   <li>the original (blocking) {@code m} untouched — the synchronous tier.</li>
 * </ul>
 *
 * <p>The rewrite at each await call site i (operand stack: {@code [below..., future]}):
 * <pre>
 *   state = i
 *   tr = getCompleted(future)            // fast path for already-completed futures
 *   if (tr != null) goto resume_i        // locals and stack still intact
 *   spill locals and below-stack into the refs/prims frame
 *   onComplete(future); return           // park
 * resume_i:                              // also targeted by the dispatch switch, after restore
 *   push tryGet(tr)                      // value, or rethrows the failure into original handlers
 * </pre>
 * The method prologue dispatches on {@code state}: case 0 restores the constructor-spilled
 * parameters; case i restores the locals and operand stack recorded for await site i and jumps
 * to {@code resume_i}.
 *
 * <p>{@code new Foo(await(f))} — an uninitialized object on the stack at the suspension point,
 * which cannot be spilled to the heap — is handled by a pre-pass ({@link #sinkUninitializedNews})
 * that deletes the {@code NEW}(+{@code DUP}) and re-materializes it just below the constructor
 * arguments at the {@code INVOKESPECIAL <init>} site, the same strategy as Kotlin coroutines.
 * (Observable difference: the class-initialization side effect of {@code NEW} moves after the
 * evaluation of the constructor arguments; Kotlin accepts the same.)
 *
 * <p>Current limitations (deliberate, see ASYNC3-DESIGN.md): static methods only; rejects
 * suspension while a monitor is held; spills all assigned locals rather than only live ones.
 */
public final class AsyncTransformer {

    public static final String AWAIT_OWNER = "async3/runtime/AsyncRT";
    public static final String AWAIT_NAME = "await";
    public static final String AWAIT_DESC = "(Ljava/util/concurrent/CompletableFuture;)Ljava/lang/Object;";

    static final String FSM = "async3/runtime/FutureStateMachine";
    static final String CF = "java/util/concurrent/CompletableFuture";
    static final String CF_DESC = "L" + CF + ";";

    public static final class Result {
        /** Binary name of the (patched) host class. */
        public String hostName;
        public byte[] hostClass;
        /** Generated state machine classes, by binary name. */
        public final Map<String, byte[]> stateMachines = new LinkedHashMap<>();
        /** Human-readable frame-layout metadata, by "binaryName.method". */
        public final Map<String, String> debugMetadata = new LinkedHashMap<>();

        public Map<String, byte[]> allClasses() {
            Map<String, byte[]> all = new LinkedHashMap<>(stateMachines);
            all.put(hostName, hostClass);
            return all;
        }
    }

    public static Result transform(byte[] classBytes) {
        ClassNode cn = new ClassNode();
        new ClassReader(classBytes).accept(cn, ClassReader.SKIP_FRAMES);

        Result result = new Result();
        result.hostName = cn.name.replace('/', '.');
        Set<String> generatedInternalNames = new HashSet<>();
        List<MethodNode> entryPoints = new ArrayList<>();
        int index = 0;
        for (MethodNode mn : cn.methods) {
            if (!hasAwait(mn)) continue;
            checkSupported(cn, mn);
            sinkUninitializedNews(cn, mn);
            String smName = cn.name + "$async$" + mn.name + "$" + index++;
            generatedInternalNames.add(smName);
            StringBuilder debug = new StringBuilder();
            byte[] smBytes = generateStateMachine(cn, mn, smName, debug);
            result.stateMachines.put(smName.replace('/', '.'), smBytes);
            result.debugMetadata.put(result.hostName + "." + mn.name, debug.toString());
            entryPoints.add(entryPoint(mn, smName));
        }
        cn.methods.addAll(entryPoints);
        ClassWriter cw = new SmAwareClassWriter(generatedInternalNames);
        cn.accept(cw);
        result.hostClass = cw.toByteArray();
        return result;
    }

    // ------------------------------------------------------------------ marker detection

    private static boolean isAwait(AbstractInsnNode insn) {
        return insn instanceof MethodInsnNode mi
                && mi.getOpcode() == INVOKESTATIC
                && mi.owner.equals(AWAIT_OWNER)
                && mi.name.equals(AWAIT_NAME)
                && mi.desc.equals(AWAIT_DESC);
    }

    private static boolean hasAwait(MethodNode mn) {
        for (AbstractInsnNode insn : mn.instructions) if (isAwait(insn)) return true;
        return false;
    }

    private static void checkSupported(ClassNode cn, MethodNode mn) {
        String where = cn.name + "." + mn.name + mn.desc;
        if ((mn.access & ACC_STATIC) == 0)
            throw new UnsupportedOperationException("only static methods are supported yet: " + where);
        if ((mn.access & ACC_SYNCHRONIZED) != 0)
            throw new UnsupportedOperationException("await is illegal in a synchronized method: " + where);
        for (AbstractInsnNode insn : mn.instructions) {
            int op = insn.getOpcode();
            // Coarse: rejects any monitor usage in an awaiting method. Precise monitor-depth
            // dataflow ("is a monitor held *at* the suspension point") is a straightforward
            // refinement.
            if (op == MONITORENTER || op == MONITOREXIT)
                throw new UnsupportedOperationException("await may not be used while a monitor is held: " + where);
            if (op == JSR || op == RET)
                throw new UnsupportedOperationException("JSR/RET not supported: " + where);
        }
    }

    // ------------------------------------------------------------------ entry point on host class

    private static MethodNode entryPoint(MethodNode mn, String smName) {
        Type[] args = Type.getArgumentTypes(mn.desc);
        MethodNode ep = new MethodNode(
                ACC_PUBLIC | ACC_STATIC, mn.name + "$async",
                Type.getMethodDescriptor(Type.getObjectType(CF), args), null, null);
        InsnList il = ep.instructions;
        il.add(new TypeInsnNode(NEW, smName));
        il.add(new InsnNode(DUP));
        int slot = 0;
        for (Type t : args) {
            il.add(new VarInsnNode(t.getOpcode(ILOAD), slot));
            slot += t.getSize();
        }
        il.add(new MethodInsnNode(INVOKESPECIAL, smName, "<init>",
                Type.getMethodDescriptor(Type.VOID_TYPE, args), false));
        il.add(new MethodInsnNode(INVOKEVIRTUAL, smName, "start", "()" + CF_DESC, false));
        il.add(new InsnNode(ARETURN));
        return ep;
    }

    // ------------------------------------------------------------------ state machine class

    private static byte[] generateStateMachine(ClassNode cn, MethodNode mn, String smName, StringBuilder debug) {
        Type[] args = Type.getArgumentTypes(mn.desc);
        Frame<BasicValue>[] frames = analyze(cn.name, mn);

        ClassNode sm = new ClassNode();
        sm.version = V17;
        sm.access = ACC_PUBLIC | ACC_FINAL | ACC_SUPER;
        sm.name = smName;
        sm.superName = FSM;
        sm.sourceFile = cn.sourceFile;

        debug.append("method ").append(cn.name).append('.').append(mn.name).append(mn.desc).append('\n');

        // Generous positional frame layout: slot v of the original method <-> frame index v;
        // operand stack entry j <-> frame index maxLocals + j. Per-state reuse is automatic.
        int frameSlots = mn.maxLocals + mn.maxStack;

        sm.methods.add(constructor(args, frameSlots));
        sm.methods.add(applyMethod(mn, frames, debug));

        sm.fields.add(new FieldNode(ACC_PUBLIC | ACC_STATIC | ACC_FINAL,
                "$asyncDebug", "Ljava/lang/String;", null, debug.toString()));

        ClassWriter cw = new SmAwareClassWriter(Set.of(smName));
        sm.accept(cw);
        return cw.toByteArray();
    }

    private static MethodNode constructor(Type[] args, int frameSlots) {
        MethodNode ctor = new MethodNode(ACC_PUBLIC, "<init>",
                Type.getMethodDescriptor(Type.VOID_TYPE, args), null, null);
        InsnList il = ctor.instructions;
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(pushInt(frameSlots));
        il.add(pushInt(frameSlots));
        il.add(new MethodInsnNode(INVOKESPECIAL, FSM, "<init>", "(II)V", false));
        // Spill constructor arguments into the frame slots of the original parameter locals;
        // the dispatch switch's case 0 restores them.
        int ctorLocal = 1, paramSlot = 0;
        for (Type t : args) {
            il.add(spillFromLocal(t, ctorLocal, paramSlot));
            ctorLocal += t.getSize();
            paramSlot += t.getSize();
        }
        il.add(new InsnNode(RETURN));
        return ctor;
    }

    private static MethodNode applyMethod(MethodNode mn, Frame<BasicValue>[] frames, StringBuilder debug) {
        MethodNode apply = new MethodNode(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)V", null, null);
        Type returnType = Type.getReturnType(mn.desc);
        Type[] args = Type.getArgumentTypes(mn.desc);

        // apply's local layout: 0 = this, 1 = tr, [2, 2+maxLocals) = original locals,
        // then a scratch slot for the awaited future and a 2-wide scratch for spills/boxing.
        final int OFF = 2;
        final int futTmp = OFF + mn.maxLocals;
        final int scratch = futTmp + 1;

        List<AbstractInsnNode> awaits = new ArrayList<>();
        for (AbstractInsnNode insn : mn.instructions) if (isAwait(insn)) awaits.add(insn);
        int n = awaits.size();

        Map<LabelNode, LabelNode> labelMap = new HashMap<>();
        for (AbstractInsnNode insn : mn.instructions)
            if (insn instanceof LabelNode l) labelMap.put(l, new LabelNode());

        LabelNode bodyStart = new LabelNode();
        LabelNode dflt = new LabelNode();
        LabelNode handler = new LabelNode();
        LabelNode[] cases = new LabelNode[n + 1];
        LabelNode[] resume = new LabelNode[n + 1];
        for (int i = 0; i <= n; i++) cases[i] = new LabelNode();
        for (int i = 1; i <= n; i++) resume[i] = new LabelNode();

        // --- transformed copy of the body
        InsnList body = new InsnList();
        int state = 0;
        for (AbstractInsnNode insn : mn.instructions) {
            if (insn instanceof LabelNode l) {
                body.add(labelMap.get(l));
            } else if (insn instanceof FrameNode) {
                // dropped; COMPUTE_FRAMES recomputes
            } else if (insn instanceof LineNumberNode ln) {
                body.add(new LineNumberNode(ln.line, labelMap.get(ln.start)));
            } else if (isAwait(insn)) {
                state++;
                Frame<BasicValue> f = frames[mn.instructions.indexOf(insn)];
                if (f == null) throw new IllegalStateException("unreachable await call");
                body.add(awaitSite(state, f, mn.maxLocals, OFF, futTmp, scratch, resume[state]));
                appendDebug(debug, mn, insn, state, f);
            } else if (insn.getOpcode() >= IRETURN && insn.getOpcode() <= RETURN) {
                body.add(returnSite(returnType, scratch));
            } else {
                AbstractInsnNode c = insn.clone(labelMap);
                if (c instanceof VarInsnNode v) v.var += OFF;
                else if (c instanceof IincInsnNode ii) ii.var += OFF;
                body.add(c);
            }
        }

        // --- dispatch prologue
        InsnList il = apply.instructions;
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(new FieldInsnNode(GETFIELD, FSM, "state", "I"));
        il.add(new TableSwitchInsnNode(0, n, dflt, cases));

        il.add(cases[0]);
        int slot = 0;
        for (Type t : args) {
            il.add(restoreToLocal(t, slot, OFF + slot));
            slot += t.getSize();
        }
        il.add(new JumpInsnNode(GOTO, bodyStart));

        for (int i = 1; i <= n; i++) {
            il.add(cases[i]);
            Frame<BasicValue> f = frames[mn.instructions.indexOf(awaits.get(i - 1))];
            for (int v = 0; v < mn.maxLocals; v++) {
                BasicValue value = f.getLocal(v);
                if (!isSpillable(value)) continue;
                il.add(restoreToLocal(value.getType(), v, OFF + v));
            }
            for (int j = 0; j < f.getStackSize() - 1; j++) {
                il.add(restorePush(f.getStack(j).getType(), mn.maxLocals + j));
            }
            il.add(new JumpInsnNode(GOTO, resume[i]));
        }

        il.add(dflt);
        il.add(new TypeInsnNode(NEW, "java/lang/IllegalStateException"));
        il.add(new InsnNode(DUP));
        il.add(new MethodInsnNode(INVOKESPECIAL, "java/lang/IllegalStateException", "<init>", "()V", false));
        il.add(new InsnNode(ATHROW));

        il.add(bodyStart);
        il.add(body);

        // --- outermost failure handler: uncaught exceptions fail the result future
        il.add(handler);
        il.add(new VarInsnNode(ASTORE, scratch));
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(new VarInsnNode(ALOAD, scratch));
        il.add(new MethodInsnNode(INVOKEVIRTUAL, FSM, "completeFailure", "(Ljava/lang/Throwable;)V", false));
        il.add(new InsnNode(RETURN));

        apply.tryCatchBlocks = new ArrayList<>();
        if (mn.tryCatchBlocks != null)
            for (TryCatchBlockNode tcb : mn.tryCatchBlocks)
                apply.tryCatchBlocks.add(new TryCatchBlockNode(
                        labelMap.get(tcb.start), labelMap.get(tcb.end), labelMap.get(tcb.handler), tcb.type));
        // appended last: user handlers (cloned above) take precedence
        apply.tryCatchBlocks.add(new TryCatchBlockNode(bodyStart, handler, handler, null));

        // The LocalVariableTable carries over: spilled values are restored into the (shifted)
        // original slots, and the resume code sits inside the original scope labels, so the
        // entries stay truthful in resumed regions — a debugger sees ordinary named locals.
        if (mn.localVariables != null)
            for (LocalVariableNode lv : mn.localVariables)
                apply.localVariables.add(new LocalVariableNode(lv.name, lv.desc, lv.signature,
                        labelMap.get(lv.start), labelMap.get(lv.end), lv.index + OFF));

        apply.maxLocals = OFF + mn.maxLocals + 3;
        apply.maxStack = mn.maxStack + 8; // recomputed by COMPUTE_FRAMES
        return apply;
    }

    /** The replacement for one await call instruction. On entry the stack is [below..., future]. */
    private static InsnList awaitSite(int state, Frame<BasicValue> f, int maxLocals,
                                      int off, int futTmp, int scratch, LabelNode resumeLabel) {
        InsnList il = new InsnList();
        LabelNode fast = new LabelNode();

        il.add(new VarInsnNode(ASTORE, futTmp));
        // state must be written before onComplete: the callback may fire on another thread
        // immediately; whenComplete registration publishes the spilled frame and state.
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(pushInt(state));
        il.add(new FieldInsnNode(PUTFIELD, FSM, "state", "I"));
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(new VarInsnNode(ALOAD, futTmp));
        il.add(new MethodInsnNode(INVOKEVIRTUAL, FSM, "getCompleted", "(" + CF_DESC + ")Ljava/lang/Object;", false));
        il.add(new InsnNode(DUP));
        il.add(new JumpInsnNode(IFNONNULL, fast));
        il.add(new InsnNode(POP));

        // park: spill assigned locals ...
        for (int v = 0; v < maxLocals; v++) {
            BasicValue value = f.getLocal(v);
            requireInitialized(value);
            if (!isSpillable(value)) continue;
            il.add(spillFromLocal(value.getType(), off + v, v));
        }
        // ... and the operand stack below the future, popped top-down via the scratch local
        for (int j = f.getStackSize() - 2; j >= 0; j--) {
            BasicValue value = f.getStack(j);
            requireInitialized(value);
            Type t = value.getType();
            if (isNullType(t)) {
                il.add(new InsnNode(POP));
            } else {
                il.add(new VarInsnNode(t.getOpcode(ISTORE), scratch));
                il.add(spillFromLocal(t, scratch, maxLocals + j));
            }
        }
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(new VarInsnNode(ALOAD, futTmp));
        il.add(new MethodInsnNode(INVOKEVIRTUAL, FSM, "onComplete", "(" + CF_DESC + ")V", false));
        il.add(new InsnNode(RETURN));

        // fast path: stack is [below..., tr]
        il.add(fast);
        il.add(new VarInsnNode(ASTORE, 1));
        // join point with the slow-path restore (which jumps here with the same shape)
        il.add(resumeLabel);
        il.add(new VarInsnNode(ALOAD, 0));
        il.add(new VarInsnNode(ALOAD, 1));
        il.add(new MethodInsnNode(INVOKEVIRTUAL, FSM, "tryGet", "(Ljava/lang/Object;)Ljava/lang/Object;", false));
        // tryGet leaves the awaited value (as Object) on the stack; the original instructions
        // that followed the await (checkcast / unboxing) consume it unchanged.
        return il;
    }

    /** Replaces a return instruction: complete the result future and exit the dispatch. */
    private static InsnList returnSite(Type returnType, int scratch) {
        InsnList il = new InsnList();
        if (returnType.getSort() == Type.VOID) {
            il.add(new VarInsnNode(ALOAD, 0));
            il.add(new InsnNode(ACONST_NULL));
        } else {
            il.add(box(returnType));
            il.add(new VarInsnNode(ASTORE, scratch));
            il.add(new VarInsnNode(ALOAD, 0));
            il.add(new VarInsnNode(ALOAD, scratch));
        }
        il.add(new MethodInsnNode(INVOKEVIRTUAL, FSM, "completeSuccess", "(Ljava/lang/Object;)V", false));
        il.add(new InsnNode(RETURN));
        return il;
    }

    // ------------------------------------------------------------------ spill/restore helpers

    private static boolean isRef(Type t) {
        return t.getSort() == Type.OBJECT || t.getSort() == Type.ARRAY;
    }

    private static boolean isNullType(Type t) {
        return t.getSort() == Type.OBJECT && t.getInternalName().equals("null");
    }

    private static boolean isSpillable(BasicValue v) {
        return v != null
                && v != BasicValue.UNINITIALIZED_VALUE
                && v.getType() != null
                && v.getType().getSort() != Type.VOID; // RETURNADDRESS
    }

    private static void requireInitialized(BasicValue v) {
        if (v instanceof UninitValue)
            throw new UnsupportedOperationException(
                    "uninitialized object on the stack at a suspension point " +
                    "(`new Foo(await(f))`): not yet supported, needs NEW-sinking");
    }

    /** Spills a value held in local {@code localIdx} of the method being built into frame slot {@code frameIdx}. */
    private static InsnList spillFromLocal(Type t, int localIdx, int frameIdx) {
        InsnList il = new InsnList();
        if (isNullType(t)) return il; // restored as ACONST_NULL
        il.add(new VarInsnNode(ALOAD, 0));
        if (isRef(t)) {
            il.add(new FieldInsnNode(GETFIELD, FSM, "refs", "[Ljava/lang/Object;"));
            il.add(pushInt(frameIdx));
            il.add(new VarInsnNode(ALOAD, localIdx));
            il.add(new InsnNode(AASTORE));
        } else {
            il.add(new FieldInsnNode(GETFIELD, FSM, "prims", "[J"));
            il.add(pushInt(frameIdx));
            switch (t.getSort()) {
                case Type.FLOAT -> {
                    il.add(new VarInsnNode(FLOAD, localIdx));
                    il.add(new MethodInsnNode(INVOKESTATIC, "java/lang/Float", "floatToRawIntBits", "(F)I", false));
                    il.add(new InsnNode(I2L));
                }
                case Type.LONG -> il.add(new VarInsnNode(LLOAD, localIdx));
                case Type.DOUBLE -> {
                    il.add(new VarInsnNode(DLOAD, localIdx));
                    il.add(new MethodInsnNode(INVOKESTATIC, "java/lang/Double", "doubleToRawLongBits", "(D)J", false));
                }
                default -> { // boolean/byte/char/short/int
                    il.add(new VarInsnNode(ILOAD, localIdx));
                    il.add(new InsnNode(I2L));
                }
            }
            il.add(new InsnNode(LASTORE));
        }
        return il;
    }

    /** Pushes the value in frame slot {@code frameIdx} onto the operand stack, typed as {@code t}. */
    private static InsnList restorePush(Type t, int frameIdx) {
        InsnList il = new InsnList();
        if (isNullType(t)) {
            il.add(new InsnNode(ACONST_NULL));
            return il;
        }
        il.add(new VarInsnNode(ALOAD, 0));
        if (isRef(t)) {
            il.add(new FieldInsnNode(GETFIELD, FSM, "refs", "[Ljava/lang/Object;"));
            il.add(pushInt(frameIdx));
            il.add(new InsnNode(AALOAD));
            if (!t.getInternalName().equals("java/lang/Object"))
                il.add(new TypeInsnNode(CHECKCAST, t.getInternalName()));
        } else {
            il.add(new FieldInsnNode(GETFIELD, FSM, "prims", "[J"));
            il.add(pushInt(frameIdx));
            il.add(new InsnNode(LALOAD));
            switch (t.getSort()) {
                case Type.FLOAT -> {
                    il.add(new InsnNode(L2I));
                    il.add(new MethodInsnNode(INVOKESTATIC, "java/lang/Float", "intBitsToFloat", "(I)F", false));
                }
                case Type.LONG -> {}
                case Type.DOUBLE ->
                    il.add(new MethodInsnNode(INVOKESTATIC, "java/lang/Double", "longBitsToDouble", "(J)D", false));
                default -> il.add(new InsnNode(L2I));
            }
        }
        return il;
    }

    private static InsnList restoreToLocal(Type t, int frameIdx, int localIdx) {
        InsnList il = restorePush(t, frameIdx);
        il.add(new VarInsnNode(isNullType(t) ? ASTORE : t.getOpcode(ISTORE), localIdx));
        return il;
    }

    private static InsnList box(Type t) {
        InsnList il = new InsnList();
        String owner = switch (t.getSort()) {
            case Type.BOOLEAN -> "java/lang/Boolean";
            case Type.BYTE -> "java/lang/Byte";
            case Type.CHAR -> "java/lang/Character";
            case Type.SHORT -> "java/lang/Short";
            case Type.INT -> "java/lang/Integer";
            case Type.FLOAT -> "java/lang/Float";
            case Type.LONG -> "java/lang/Long";
            case Type.DOUBLE -> "java/lang/Double";
            default -> null;
        };
        if (owner != null)
            il.add(new MethodInsnNode(INVOKESTATIC, owner, "valueOf",
                    "(" + t.getDescriptor() + ")L" + owner + ";", false));
        return il;
    }

    private static AbstractInsnNode pushInt(int v) {
        if (v >= -1 && v <= 5) return new InsnNode(ICONST_0 + v);
        if (v >= Byte.MIN_VALUE && v <= Byte.MAX_VALUE) return new IntInsnNode(BIPUSH, v);
        if (v >= Short.MIN_VALUE && v <= Short.MAX_VALUE) return new IntInsnNode(SIPUSH, v);
        return new LdcInsnNode(v);
    }

    // ------------------------------------------------------------------ NEW-sinking pre-pass

    /**
     * Rewrites {@code NEW T; [DUP;] ...await...; INVOKESPECIAL T.<init>} so that no uninitialized
     * reference is on the operand stack at a suspension point: the {@code NEW}(+{@code DUP}) is
     * deleted from its original position and re-materialized just below the constructor arguments
     * at the {@code <init>} site (args are briefly parked in fresh scratch locals to make room).
     * Handles the javac idiom, including nested news, multiple awaits among the arguments, and
     * conditional argument expressions; anything more exotic (extra stack-shuffled copies,
     * an uninitialized reference stored in a local) is rejected by {@link #requireInitialized}.
     */
    private static void sinkUninitializedNews(ClassNode cn, MethodNode mn) {
        Frame<BasicValue>[] frames = analyze(cn.name, mn);
        AbstractInsnNode[] insns = mn.instructions.toArray();

        // uninitialized values in flight at any suspension point, deduped by their NEW insn
        Set<UninitValue> pending = new LinkedHashSet<>();
        for (int i = 0; i < insns.length; i++) {
            if (!isAwait(insns[i]) || frames[i] == null) continue;
            for (int j = 0; j < frames[i].getStackSize(); j++)
                if (frames[i].getStack(j) instanceof UninitValue u) pending.add(u);
        }
        if (pending.isEmpty()) return;

        String where = cn.name + "." + mn.name + mn.desc;
        for (UninitValue u : pending) {
            AbstractInsnNode afterNew = nextSignificant(u.newInsn);
            boolean dupped = afterNew != null && afterNew.getOpcode() == DUP;

            // locate the <init> consuming u as its receiver (frames/indices are pre-rewrite,
            // but the <init> nodes themselves stay in place as insertion anchors)
            MethodInsnNode init = null;
            Frame<BasicValue> initFrame = null;
            for (int i = 0; i < insns.length; i++) {
                if (insns[i] instanceof MethodInsnNode mi && mi.getOpcode() == INVOKESPECIAL
                        && mi.name.equals("<init>") && frames[i] != null) {
                    int r = frames[i].getStackSize() - 1 - Type.getArgumentTypes(mi.desc).length;
                    if (r >= 0 && u.equals(frames[i].getStack(r))) { init = mi; initFrame = frames[i]; break; }
                }
            }
            if (init == null)
                throw new UnsupportedOperationException("cannot locate <init> for NEW at a suspension point: " + where);

            int nargs = Type.getArgumentTypes(init.desc).length;
            int receiver = initFrame.getStackSize() - 1 - nargs;
            int copies = 1;
            while (receiver - copies >= 0 && u.equals(initFrame.getStack(receiver - copies))) copies++;
            if (copies > 2 || (copies == 2) != dupped)
                throw new UnsupportedOperationException("unsupported NEW/DUP shape at a suspension point: " + where);

            Type[] argTypes = new Type[nargs];
            int[] argSlots = new int[nargs];
            int slot = mn.maxLocals;
            for (int a = 0; a < nargs; a++) {
                argTypes[a] = initFrame.getStack(receiver + 1 + a).getType();
                argSlots[a] = slot;
                slot += argTypes[a].getSize();
            }
            mn.maxLocals = slot;

            InsnList patch = new InsnList();
            for (int a = nargs - 1; a >= 0; a--)
                patch.add(new VarInsnNode(argTypes[a].getOpcode(ISTORE), argSlots[a]));
            patch.add(new TypeInsnNode(NEW, u.newInsn.desc));
            if (dupped) patch.add(new InsnNode(DUP));
            for (int a = 0; a < nargs; a++)
                patch.add(new VarInsnNode(argTypes[a].getOpcode(ILOAD), argSlots[a]));
            mn.instructions.insertBefore(init, patch);
            if (dupped) mn.instructions.remove(afterNew);
            mn.instructions.remove(u.newInsn);
        }
        mn.maxStack += 2;
    }

    private static AbstractInsnNode nextSignificant(AbstractInsnNode insn) {
        for (AbstractInsnNode p = insn.getNext(); p != null; p = p.getNext())
            if (!(p instanceof LabelNode || p instanceof LineNumberNode || p instanceof FrameNode)) return p;
        return null;
    }

    // ------------------------------------------------------------------ analysis

    private static Frame<BasicValue>[] analyze(String owner, MethodNode mn) {
        Analyzer<BasicValue> analyzer = new Analyzer<>(new Interp()) {
            @Override protected Frame<BasicValue> newFrame(int numLocals, int numStack) {
                return new InitTrackingFrame(numLocals, numStack);
            }
            @Override protected Frame<BasicValue> newFrame(Frame<? extends BasicValue> frame) {
                return new InitTrackingFrame(frame);
            }
        };
        try {
            return analyzer.analyze(owner, mn);
        } catch (AnalyzerException e) {
            throw new IllegalStateException("analysis of " + owner + "." + mn.name + " failed", e);
        }
    }

    /**
     * SimpleVerifier (precise reference types, needed for the CHECKCASTs on restore) extended to
     * mark the result of NEW as uninitialized-until-&lt;init&gt;, so suspension points with such a
     * value in flight can be rejected instead of miscompiled.
     */
    static final class Interp extends SimpleVerifier {
        Interp() {
            super(ASM9, null, null, null, false);
            setClassLoader(AsyncTransformer.class.getClassLoader());
        }

        @Override
        public BasicValue newOperation(AbstractInsnNode insn) throws AnalyzerException {
            BasicValue v = super.newOperation(insn);
            return insn.getOpcode() == NEW ? new UninitValue(v.getType(), (TypeInsnNode) insn) : v;
        }
    }

    /**
     * Value marking "allocated by {@code newInsn} but &lt;init&gt; not yet called". Equality is
     * keyed on the NEW instruction (not object identity): the analyzer re-executes blocks while
     * iterating to a fixpoint, creating fresh instances for the same allocation site, and merge
     * must recognize them as the same value or the marking is lost.
     */
    static final class UninitValue extends BasicValue {
        final TypeInsnNode newInsn;

        UninitValue(Type type, TypeInsnNode newInsn) {
            super(type);
            this.newInsn = newInsn;
        }

        @Override public boolean equals(Object o) { return o instanceof UninitValue u && u.newInsn == newInsn; }
        @Override public int hashCode() { return System.identityHashCode(newInsn); }
    }

    /**
     * Frame that, upon INVOKESPECIAL &lt;init&gt;, replaces all copies of the receiver's
     * {@link UninitValue} (e.g. the one left by DUP, or stored in a local) with an initialized
     * value — mirroring what the real verifier does. Without this, any object constructed before
     * an await and live across it would be spuriously rejected.
     */
    static final class InitTrackingFrame extends Frame<BasicValue> {
        InitTrackingFrame(int numLocals, int numStack) { super(numLocals, numStack); }
        InitTrackingFrame(Frame<? extends BasicValue> frame) { super(frame); }

        @Override
        public void execute(AbstractInsnNode insn, Interpreter<BasicValue> interpreter) throws AnalyzerException {
            BasicValue uninit = null;
            if (insn.getOpcode() == INVOKESPECIAL && insn instanceof MethodInsnNode mi && "<init>".equals(mi.name)) {
                int receiver = getStackSize() - 1 - Type.getArgumentTypes(mi.desc).length;
                if (receiver >= 0 && getStack(receiver) instanceof UninitValue u) uninit = u;
            }
            super.execute(insn, interpreter);
            if (uninit != null) {
                BasicValue inited = new BasicValue(uninit.getType());
                for (int i = 0; i < getLocals(); i++)
                    if (uninit.equals(getLocal(i))) setLocal(i, inited);
                int size = getStackSize();
                BasicValue[] stack = new BasicValue[size];
                for (int i = size - 1; i >= 0; i--) stack[i] = pop();
                for (int i = 0; i < size; i++) push(uninit.equals(stack[i]) ? inited : stack[i]);
            }
        }
    }

    // ------------------------------------------------------------------ debug metadata

    private static void appendDebug(StringBuilder sb, MethodNode mn, AbstractInsnNode awaitInsn,
                                    int state, Frame<BasicValue> f) {
        int line = -1;
        for (AbstractInsnNode p = awaitInsn; p != null; p = p.getPrevious())
            if (p instanceof LineNumberNode ln) { line = ln.line; break; }
        sb.append("state ").append(state);
        if (line >= 0) sb.append(" (line ").append(line).append(")");
        sb.append(":");
        int awaitIdx = mn.instructions.indexOf(awaitInsn);
        boolean first = true;
        for (int v = 0; v < mn.maxLocals; v++) {
            BasicValue value = f.getLocal(v);
            if (!isSpillable(value) || isNullType(value.getType())) continue;
            String name = localName(mn, v, awaitIdx);
            sb.append(first ? " " : " | ").append(name != null ? name : "local" + v)
              .append(" -> ").append(slotName(value.getType(), v));
            first = false;
        }
        for (int j = 0; j < f.getStackSize() - 1; j++) {
            Type t = f.getStack(j).getType();
            if (isNullType(t)) continue;
            sb.append(first ? " " : " | ").append("stack").append(j)
              .append(" -> ").append(slotName(t, mn.maxLocals + j));
            first = false;
        }
        sb.append('\n');
    }

    private static String slotName(Type t, int idx) {
        return (isRef(t) ? "refs[" : "prims[") + idx + "] (" + t.getDescriptor() + ")";
    }

    private static String localName(MethodNode mn, int slot, int insnIdx) {
        if (mn.localVariables == null) return null;
        for (LocalVariableNode lv : mn.localVariables) {
            if (lv.index != slot) continue;
            int s = mn.instructions.indexOf(lv.start), e = mn.instructions.indexOf(lv.end);
            if (insnIdx >= s && insnIdx < e) return lv.name;
        }
        return null;
    }

    // ------------------------------------------------------------------ class writing

    /**
     * COMPUTE_FRAMES needs a common-superclass oracle; generated state machine names are not
     * loadable by the writer's class loader, so resolve them as their superclass.
     */
    static final class SmAwareClassWriter extends ClassWriter {
        private final Set<String> generatedInternalNames;

        SmAwareClassWriter(Set<String> generatedInternalNames) {
            super(COMPUTE_FRAMES | COMPUTE_MAXS);
            this.generatedInternalNames = generatedInternalNames;
        }

        @Override
        protected String getCommonSuperClass(String type1, String type2) {
            String a = generatedInternalNames.contains(type1) ? FSM : type1;
            String b = generatedInternalNames.contains(type2) ? FSM : type2;
            if (a.equals(b)) return a;
            return super.getCommonSuperClass(a, b);
        }
    }
}
