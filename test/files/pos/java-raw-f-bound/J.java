interface IEnum {


}
class DescriptiveEnum implements IEnum{

}

abstract class TypedEnum<V, P extends ResultKey> extends DescriptiveEnum implements ResultKey<V, P>, IEnum {

}

interface ResultKey<V, P extends ResultKey> extends java.io.Serializable {

}

class ReferenceDataEnum<V> extends TypedEnum<V, Column> {
  TypedEnum asRawTypedEnum() { return this; }
  TypedEnum<V, Column> asTypedEnum() { return this; }
}

interface Column<T> extends ResultKey<T, Column> {

}

class J {
  static void m(TypedEnum te) {

  }
  static void m(TypedEnum<String, ?> te, int i) {
  }
}
