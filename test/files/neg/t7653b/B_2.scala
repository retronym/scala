object Test {
  M.m.apply() // error in qualifier correctly reported
  M.m()       // but here it resulted in a NPE
}
