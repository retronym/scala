/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.runtime;


public class ObjectRef<T> implements java.io.Serializable {
    private static final long serialVersionUID = -9055728157600312291L;

    public T elem;
    public ObjectRef(T elem) { this.elem = elem; }
    public String toString() { return String.valueOf(elem); }

    public static <U> ObjectRef create(U e) { return new ObjectRef(e); }
    public static ObjectRef zero() { return new ObjectRef(null); }
}
