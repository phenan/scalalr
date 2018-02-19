# ScaLALR : Type-Safe Syntax Extension in Scala

ScaLALR is a parser generator but it generates just a library in Scala.
You can define and use your own syntax in your Scala project. 
ScaLALR emulates LALR(1) parsing by exploiting type checking in Scala.

## Quick tour

The following program is an example of a definition of DSL syntax by using ScaLALR.

```scala
import com.phenan.scalalr._

@dsl[JValue]
object JSONSyntax {
  @syntax(s"[ $values ]")
  def jArray (values: JValue@sep(",")*): JArray = JArray(values.toList)
  
  @syntax(s"{ $fields }")
  def jObject (fields: JField@sep(",")*): JObject = JObject(fields.toList)
  
  @syntax(s"$name : $value")
  def jField (name: String, value: JValue): JField = JField(name, value)
  
  @syntax(s"$value")
  def jDouble (value: Double): JDouble = JDouble(value)
  
  @syntax(s"$value")
  def jLong (value: Long): JLong = JLong(value)
  
  @syntax(s"$value")
  def jBool (value: Boolean): JBool = JBool(value)
  
  @syntax(s"$value")
  def jString (value: String): JString = JString(value)
}
```

This definition allows you to write JSON-like program as follows:

```scala
import com.phenan.scalalr._
import scala.language.postfixOps
import JSONSyntax._

val doubleValue: JValue = (10.0)

val jsonArray: JValue = $$bracketleft (10.0)$$comma ("hello") $$bracketright

val jsonObject: JValue = (
  $$braceleft
    ("foo") $$colon (false)$$comma
    ("bar") $$colon $$bracketleft ("baz")$$comma (20.0) $$bracketright
  $$braceright
)
```

This code looks quite strange, however,
if you use our custom font that supports several ligatures,
the code is displayed as follows:

<img width="387" alt="2018-02-08 16 34 20" src="https://user-images.githubusercontent.com/4749268/35960681-f362da7c-0ced-11e8-8053-5baa0c7752cf.png">

## Installation

This project currently supports Scala 2.12.
You should add the following to your `build.sbt`.

```sbtshell
  resolvers += Resolver.jcenterRepo
  
  libraryDependencies += "com.phenan" %% "scalalr" % "2.3.2"
  
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

We provide a custom font for ScaLALR, named Scalig.
The font file is placed in `font` directory.

If you want to fully use ScaLALR, 
please change the font to Scalig and enable the font ligatures of your editor.


## DSL syntax definition

### Annotate `@dsl` to the declaration of a singleton object

To define your own DSL, you declare a singleton object with `@dsl` annotation.
The `@dsl` annotation takes a type argument.
The given type argument expresses that this is a DSL for a value of the type.

```scala
@dsl[JValue]
object JSONSyntax {
  // declarations of DSL
}
```

### Define DSL syntax by declaring methods with `@syntax` annotation

You can define syntax by attaching `@syntax` annotation to a method declaration 
in the body of the singleton object.
`@syntax` annotation takes an argument of the form `s"..."` that expresses the syntax of the method.
For example, look at the following declaration:

```scala
@syntax(s"$name : $value")
def jField (name: String, value: JValue): JField = JField(name, value)
```

Here, `$name : $value` indicates the syntax.
An identifier preceded by `$` such as `$name` indicates an argument part of the syntax
and the identifier corresponds to a parameter name.
So this declaration means that you can use the syntax like `x : y` and 
`jField(x, y)` is called in this case.
Of course, such the syntax is available only at the limited expression.
This syntax is only available at the expression that expects a value of `JField`.
Like this, you can define your own DSL by declaring syntax for each types.


### Another use case of `@syntax` annotation

Unfortunately, `@syntax` annotation causes an error reporting in IntelliJ.
(In fact, `@syntax` annotation causes a compilation error if you use it without `@dsl` annotation.)
So we enable the `@syntax` annotation to attach to the return type of functions as follows:

```scala
def jField (name: String, value: JValue): JField @syntax(s"$name : $value") = JField(name, value)
```

This does not cause an error reporting in IntelliJ, because `name` and `value` can be referred from there.


If you define your own types for your DSL, 
you can declare your syntax by attaching `@syntax` annotations to your class definitions instead of methods.
The following is an example:

```scala
@dsl[Math.Expr]
object Math {
  sealed trait Expr

  @syntax(s"$n + $m")
  case class Add (n: Expr, m: Int) extends Expr

  @syntax(s"$n - $m")
  case class Sub (n: Expr, m: Int) extends Expr

  @syntax(s"$n")
  case class Num (n: Int) extends Expr
}
```

### Expressing repetitions

To express a repetition, you can use variable arguments and `@sep` annotation.
For example, the following uses variable arguments with `@sep` annotation.

```scala
@syntax(s"[ $values ]")
def jArray (values: JValue@sep(",")*): JArray = JArray(values.toList)
```

This expresses the syntax that recognizes
`[]`, `[a]`, `[a, b]`, `[a, b, c]`, and so on.
`$values` in the `@syntax` annotation indicates an argument
and the corresponding parameter `values` is a parameter taking variable arguments.
The element type of the variable arguments is `JValue@sep(",")`, 
which means that the argument takes zero or more values of `JValue` and each arguments are separated by `,`.


## Using DSLs

If you want to use your DSL, 
you should import all members of your singleton object that you have declared with `@dsl` annotation.
You must also import `com.phenan.scalalr._` and `scala.language.postfixOps` to use DSLs defined in ScaLALR.

```scala
import com.phenan.scalalr._
import scala.language.postfixOps
import JSONSyntax._
```

Now, you can use your DSL syntax.
To use the DSL, you have to explicitly write the return type of a DSL program.

If your DSL program is a single line program,
the next line of the program should be a blank line.
If your DSL program is a multiline,
you should enclose your program with parentheses.

```scala
val doubleValue: JValue = (10.0)

val jsonArray: JValue = $$bracketleft (10.0)$$comma ("hello") $$bracketright

val jsonObject: JValue = (
  $$braceleft
    ("foo") $$colon (false)$$comma
    ("bar") $$colon $$bracketleft ("baz")$$comma (20.0) $$bracketright
  $$braceright
)
```


You can use a Scala expression as a part of a DSL program.
To do so, you should enclose the Scala expression with parentheses.
You can write a Scala code enclosed by parentheses in any argument part of DSL syntax.


Symbols in the syntax of the DSL are automatically translated into other identifiers as shown below.
Our font, Scalig, provides ligatures that make these identifiers look the same as the original symbols.


| symbol | identifier |
|:------:|------------|
| !      | $$exclam          |
| "      | $$quotedbl          |
| #      | $$numbersign          |
| %      | $$percent          |
| &      | $$ampersand          |
| '      | $$quotesingle          |
| (      | $$parenleft          |
| )      | $$parenright          |
| *      | $$asterisk          |
| +      | $$plus          |
| ,      | $$comma          |
| -      | $$hyphen          |
| .      | $$period          |
| /      | $$slash          |
| :      | $$colon          |
| ;      | $$semicolon          |
| <      | $$less          |
| =      | $$equal          |
| &gt;   | $$greater          |
| ?      | $$question          |
| @      | $$at          |
| [      | $$bracketleft          |
| \      | $$backslash          |
| ]      | $$bracketright          |
| ^      | $$asciicircum          |
| `      | $$grave          |
| {      | $$braceleft          |
| &#124; | $$bar          |
| }      | $$braceright          |
| ~      | $$asciitilde          |
  



## Author

[@phenan](https://twitter.com/phenan)
