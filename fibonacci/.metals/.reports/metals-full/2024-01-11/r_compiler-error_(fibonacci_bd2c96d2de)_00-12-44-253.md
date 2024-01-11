file://<WORKSPACE>/Fib.scala
### java.lang.StringIndexOutOfBoundsException: begin 972, end 971, length 1009

occurred in the presentation compiler.

action parameters:
offset: 971
uri: file://<WORKSPACE>/Fib.scala
text:
```scala

import java.io._

object Fib {
  def fib(n: BigInt): BigInt = {
    val sign = if (n.signum >= 0) 1 else -1
    BigInt(sign).pow(1 + n.abs.toInt) * pow(n.abs.toInt)(1)(0)
  }

  def pow(n: BigInt): Array[Array[BigInt]] = n match {
    case x if x == 0 => Array(Array(BigInt(1), BigInt(0)), Array(BigInt(0), BigInt(1)))
    case x if x == 1 => Array(Array(BigInt(0), BigInt(1)), Array(BigInt(1), BigInt(1)))
    case x if x % 2 == 0 =>
      val a = pow(x / 2)
      Array(
        Array(a(0)(0).pow(2) + a(0)(1) * a(1)(0), a(0)(1) * (a(0)(0) + a(1)(1))),
        Array(a(1)(0) * (a(0)(0) + a(1)(1)), a(1)(1).pow(2) + a(0)(1) * a(1)(0))
      )
    case _ =>
      val b = pow(n - 1)
      Array(
          Array(b(0)(1), b(0)(0) + b(0)(1)),
          Array(b(1)(1), b(1)(0) + b(1)(1))
        )
  }

  def main(args: Array[String]): Unit = {
    val pw = new PrintWriter(new File("output.txt" ))
    for(arg<-args) {
      val result = fib(arg.toInt)
      pw.write(s"\n@@$result\n")
    }
    pw.close()
  }
}
```



#### Error stacktrace:

```
java.base/java.lang.String.checkBoundsBeginEnd(String.java:4606)
	java.base/java.lang.String.substring(String.java:2709)
	scala.meta.internal.pc.InterpolationSplice$.apply(InterpolationSplice.scala:40)
	scala.meta.internal.pc.completions.InterpolatorCompletions$.contribute(InterpolatorCompletions.scala:38)
	scala.meta.internal.pc.completions.Completions.advancedCompletions(Completions.scala:486)
	scala.meta.internal.pc.completions.Completions.completions(Completions.scala:183)
	scala.meta.internal.pc.completions.CompletionProvider.completions(CompletionProvider.scala:86)
	scala.meta.internal.pc.ScalaPresentationCompiler.complete$$anonfun$1(ScalaPresentationCompiler.scala:136)
```
#### Short summary: 

java.lang.StringIndexOutOfBoundsException: begin 972, end 971, length 1009