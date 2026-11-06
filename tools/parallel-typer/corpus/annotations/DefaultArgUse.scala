package annots

class DefaultArgUse {
  @DefaultArgAnn def a = 1
  @DefaultArgAnn("x") def b = 2
  @DefaultArgAnn(n = 2) def c = 3
}
