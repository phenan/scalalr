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
  
  libraryDependencies += "com.phenan" %% "scalalr" % "2.3.1"
  
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
```

We provide a custom font for ScaLALR, named Scalig.
The font file is placed in `font` directory.

If you want to fully use ScaLALR, 
please change the font to Scalig and enable the font ligatures of your editor.


## DSL syntax definition

...

## Using DSLs

...

## Author

[@phenan](https://twitter.com/phenan)
