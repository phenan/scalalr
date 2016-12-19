# ScaLALR : LALR parser generator for embedded DSLs in Scala

ScaLALR is a parser generator but it generates just a library in Scala.
You can use your own syntax in your Scala project.
 
ScaLALR emulates LALR(1) parsing by expressing shift & reduce operations by implicit conversions and implicit parameters.
So it can express any LR languages but all terminals should be an identifier in Scala.

## Usage

### write syntax file

```
syntax MathDSL (program) {
  program = stmts | stmt    ;
  stmts = stmt program      ;
  stmt = expr "end"         ;
  expr = add | sub | term   ;
  add = expr "plus" term    ;
  sub = expr "minus" term   ;
  term = mul | div | factor ;
  mul = term "mul" factor   ; 
  div = term "div" factor   ; 
  factor = paren | num      ; 
  paren = "lp" expr "rp"    ; 
  num = int                 ; 
}
```

The source code above declares syntax named `MathDSL`.
The start symbol of this syntax is `program`.
The syntax rules are written in the following curly-braced code block.
Each rules are separated by semicolons.
Identifiers such as `program` and `stmts` are a non-terminal symbol.
Double-quoted strings such as `"end"` and `"plus"` are a terminal symbol.
`int` (or `id`) is a pre-defined terminal symbol that recognizes `int(<int value>)` (or `id(<string value>)`).
For easy to analyze, a rule including "or" rule `|` must be a simple rule as follows:

```
  expr = add | sub | term
```

`|` cannot take a complex parsing expression as its operands.


### run our parser generator

```
  $ sbt "run <syntax file>"
```

If you run our parser generator, it outputs a source program in Scala to the standard output.
You can use the syntax you defined in your scala program by importing the output program.
To do this, you should copy and paste the output program into a .scala file in your project.
The following example is an use case of the syntax that defined above.

```
object Main {
  import MathDSL._
  def main (args: Array[String]): Unit = {
    val program: Program = lp int(10) plus int(2) rp mul lp int(10) div int(5) rp end
    println(program)
  }
}
```

## Author

[@phenan](https://twitter.com/phenan)
