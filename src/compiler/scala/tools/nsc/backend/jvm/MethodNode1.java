/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.backend.jvm;

import scala.tools.asm.Label;
import scala.tools.asm.Opcodes;
import scala.tools.asm.tree.LabelNode;
import scala.tools.asm.tree.MethodNode;
/**
 * A subclass of {@link MethodNode} to customize the representation of
 * label nodes with {@link LabelNode1}.
 */
public class MethodNode1 extends MethodNode {
    public MethodNode1(int api, int access, String name, String descriptor, String signature, String[] exceptions) {
        super(api, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int access, String name, String descriptor, String signature, String[] exceptions) {
        this(Opcodes.ASM6, access, name, descriptor, signature, exceptions);
    }

    public MethodNode1(int api) {
        super(api);
    }

    public MethodNode1() {
        this(Opcodes.ASM6);
    }

    @Override
    protected LabelNode getLabelNode(Label label) {
        if (!(label.info instanceof LabelNode)) {
            label.info = new LabelNode1(label);
        }
        return (LabelNode) label.info;
    }

    private void markJumpTarget(Label label) {
        ((LabelNode1) getLabelNode(label)).flags |= LabelNode1.JUMP_TARGET;
    }

    @Override
    public void visitJumpInsn(int opcode, Label label) {
        markJumpTarget(label);
        super.visitJumpInsn(opcode, label);
    }

    @Override
    public void visitTableSwitchInsn(int min, int max, Label dflt, Label... labels) {
        markJumpTarget(dflt);
        for (Label label : labels) markJumpTarget(label);
        super.visitTableSwitchInsn(min, max, dflt, labels);
    }

    @Override
    public void visitLookupSwitchInsn(Label dflt, int[] keys, Label[] labels) {
        markJumpTarget(dflt);
        for (Label label : labels) markJumpTarget(label);
        super.visitLookupSwitchInsn(dflt, keys, labels);
    }
}
