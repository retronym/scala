@interface A { int value(); }

class UsesA { @A(1) int f; }
