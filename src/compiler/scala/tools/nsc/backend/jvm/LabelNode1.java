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
import scala.tools.asm.tree.ClassNode;
import scala.tools.asm.tree.LabelNode;

/**
 * A subclass of {@link LabelNode} to add user-definable flags.
 */
public class LabelNode1 extends LabelNode {
    public LabelNode1() {
    }

    public LabelNode1(Label label) {
        super(label);
    }

    /** Set by {@link MethodNode1} when a jump or switch that targets this label is visited. */
    public static final int JUMP_TARGET = 1 << 0;
    /** Set by codegen: this label is never the target of a jump visited after the label itself. */
    public static final int FORWARD_JUMPS_ONLY = 1 << 1;

    public int flags;
}
