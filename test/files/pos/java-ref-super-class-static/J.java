class J {
	static class Base {
		static class StaticInner {}
		       class       Inner {}
	}
	static class Sub extends Base {
		void f1(           Inner inner) {}

		void f2(     StaticInner inner) {} // not found: "StaticInner"
		void f3(Base.StaticInner inner) {} // workaround: qualifiy
	}
}
