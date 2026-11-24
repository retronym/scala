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

package scala.tools.nsc.jpms;

import javax.tools.Diagnostic;
import javax.tools.DiagnosticListener;

class NullDiagnosticListener<S> implements DiagnosticListener<S> {
    @Override
    public void report(Diagnostic<? extends S> diagnostic) {
    }
}
