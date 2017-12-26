package com.phenan

package object scalalr {
  implicit class GrammarStringContext (sc: StringContext) {
    def g (args: Any*): Grammar = Grammar(sc.parts, args)
  }
}
