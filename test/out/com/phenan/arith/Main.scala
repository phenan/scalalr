package com.phenan.arith

import com.phenan.scalalr._

import scala.language.postfixOps

object Main {
  import MathDSL._

  def main (args: Array[String]): Unit = {

    val program: Program = literal(10) $$plus literal(2) $$asterisk $$parenleft literal(10) $$slash literal(5) $$parenright

    println(program)

    val program2: Program = literal(10).$$plus.literal(2).$$asterisk.$$parenleft.literal(10).$$slash.literal(5).$$parenright

    println(program2)

    val program3: Program = literal(10) $$plus (2) $$asterisk $$parenleft literal(10) $$slash (5) $$parenright

    println(program3)

    val program4: Program = (10) $$plus (2) $$asterisk $$parenleft (10) $$slash (5) $$hyphen (8) $$slash (2) $$parenright

    println(program4)

    val longProgram: Program =
      1 $$plus 2 $$plus 3 $$plus 4 $$plus 5 $$plus 6 $$plus 7 $$plus 8 $$plus 9 $$plus 10 $$plus
      11 $$plus 12 $$plus 13 $$plus 14 $$plus 15 $$plus 16 $$plus 17 $$plus 18 $$plus 19 $$plus 20 $$plus
      21 $$plus 22 $$plus 23 $$plus 24 $$plus 25 $$plus 26 $$plus 27 $$plus 28 $$plus 29 $$plus 30 $$plus
      31 $$plus 32 $$plus 33 $$plus 34 $$plus 35 $$plus 36 $$plus 37 $$plus 38 $$plus 39 $$plus 40 $$plus
      41 $$plus 42 $$plus 43 $$plus 44 $$plus 45 $$plus 46 $$plus 47 $$plus 48 $$plus 49 $$plus 50 $$plus
      51 $$plus 52 $$plus 53 $$plus 54 $$plus 55 $$plus 56 $$plus 57 $$plus 58 $$plus 59 $$plus 60 $$plus
      61 $$plus 62 $$plus 63 $$plus 64 $$plus 65 $$plus 66 $$plus 67 $$plus 68 $$plus 69 $$plus 70 $$plus
      71 $$plus 72 $$plus 73 $$plus 74 $$plus 75 $$plus 76 $$plus 77 $$plus 78 $$plus 79 $$plus 80 $$plus
      81 $$plus 82 $$plus 83 $$plus 84 $$plus 85 $$plus 86 $$plus 87 $$plus 88 $$plus 89 $$plus 90 $$plus
      91 $$plus 92 $$plus 93 $$plus 94 $$plus 95 $$plus 96 $$plus 97 $$plus 98 $$plus 99 $$plus 100 $$plus
      101 $$plus 102 $$plus 103 $$plus 104 $$plus 105 $$plus 106 $$plus 107 $$plus 108 $$plus 109 $$plus 110 $$plus
      111 $$plus 112 $$plus 113 $$plus 114 $$plus 115 $$plus 116 $$plus 117 $$plus 118 $$plus 119 $$plus 120 $$plus
      121 $$plus 122 $$plus 123 $$plus 124 $$plus 125 $$plus 126 $$plus 127 $$plus 128 $$plus 129 $$plus 130 $$plus
      131 $$plus 132 $$plus 133 $$plus 134 $$plus 135 $$plus 136 $$plus 137 $$plus 138 $$plus 139 $$plus 140 $$plus
      141 $$plus 142 $$plus 143 $$plus 144 $$plus 145 $$plus 146 $$plus 147 $$plus 148 $$plus 149 $$plus 150 $$plus
      151 $$plus 152 $$plus 153 $$plus 154 $$plus 155 $$plus 156 $$plus 157 $$plus 158 $$plus 159 $$plus 160 $$plus
      161 $$plus 162 $$plus 163 $$plus 164 $$plus 165 $$plus 166 $$plus 167 $$plus 168 $$plus 169 $$plus 170 $$plus
      171 $$plus 172 $$plus 173 $$plus 174 $$plus 175 $$plus 176 $$plus 177 $$plus 178 $$plus 179 $$plus 180 $$plus
      181 $$plus 182 $$plus 183 $$plus 184 $$plus 185 $$plus 186 $$plus 187 $$plus 188 $$plus 189 $$plus 190 $$plus
      191 $$plus 192 $$plus 193 $$plus 194 $$plus 195 $$plus 196 $$plus 197 $$plus 198 $$plus 199 $$plus 200 $$plus
      201 $$plus 202 $$plus 203 $$plus 204 $$plus 205 $$plus 206 $$plus 207 $$plus 208 $$plus 209 $$plus 210 $$plus
      211 $$plus 212 $$plus 213 $$plus 214 $$plus 215 $$plus 216 $$plus 217 $$plus 218 $$plus 219 $$plus 220 $$plus
      221 $$plus 222 $$plus 223 $$plus 224 $$plus 225 $$plus 226 $$plus 227 $$plus 228 $$plus 229 $$plus 230 $$plus
      231 $$plus 232 $$plus 233 $$plus 234 $$plus 235 $$plus 236 $$plus 237 $$plus 238 $$plus 239 $$plus 240 $$plus
      241 $$plus 242 $$plus 243 $$plus 244 $$plus 245 $$plus 246 $$plus 247 $$plus 248 $$plus 249 $$plus 250 $$plus
      251 $$plus 252 $$plus 253 $$plus 254 $$plus 255 $$plus 256 $$plus 257 $$plus 258 $$plus 259 $$plus 260 $$plus
      261 $$plus 262 $$plus 263 $$plus 264 $$plus 265 $$plus 266 $$plus 267 $$plus 268 $$plus 269 $$plus 270 $$plus
      271 $$plus 272 $$plus 273 $$plus 274 $$plus 275 $$plus 276 $$plus 277 $$plus 278 $$plus 279 $$plus 280 $$plus
      281 $$plus 282 $$plus 283 $$plus 284 $$plus 285 $$plus 286 $$plus 287 $$plus 288 $$plus 289 $$plus 290 $$plus
      291 $$plus 292 $$plus 293 $$plus 294 $$plus 295 $$plus 296 $$plus 297 $$plus 298 $$plus 299 $$plus 300

    println(longProgram)
  }
}
