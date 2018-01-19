package com.phenan.scalalr

case class Shift [T, N1, N2] (shift: (N1, T) => N2)
case class Reduce [T, N1, N2] (reduce: N1 => N2)
case class Accept [NX, R] (accept: NX => R)
case class Transition [T, N1, N2] (transit: (N1, T) => N2)
case class Transitions [L <: TokenList, N1, N2] (transit: (N1, L) => N2)
