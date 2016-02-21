package org.apache.felix.dm.scala.test

import org.apache.felix.dm.scala.DependencyActivatorBase

import org.scalatest.tools.Runner
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class Activator extends DependencyActivatorBase
{
  def init() = {
    // initialize some random components
    component(new Comp1()) {
      _.provides(classOf[S1], "name" -> "comp1")
    }

    component(classOf[Comp2]) {
      _.provides(classOf[S2])
       .requires(classOf[S1]) {
         _.filter("name", "comp1")
          .added((inst,s) => inst.bind(s))
       }
       .optionally(classOf[S1])(_.filter("name", "whatever"))
       .start(_.start)
    }

    // inject services to ourself so we can start the actual test
    component(this) {
      _.requires(classOf[S1]) {
         _.added((inst,s) => inst.bind(s))
       }
       .requires(classOf[S2]) {
         _.added((inst,s) => inst.bind(s))
       }
       .start(_.start)
    }
  }

  def bind(s:S1) = TestDependencies.s1 = Some(s)
  def bind(s:S2) = TestDependencies.s2 = Some(s)

  def start:Unit = Future {
    assert( Runner.run(Array("-o",
      "-u", System.getProperty("test.out"),
      "-s", classOf[DMSpec].getName)) )
  } onComplete {
    case Success(_) => System.exit(0)
    case Failure(_)  => System.exit(1)
  }
}

trait S1
class Comp1 extends S1

trait S2
class Comp2 extends S2
{
  private var s1:S1 = _ // injected by callback
  //private var o1:Option[S1] = None // injected by field

  def bind(s:S1) = s1 = s
  def unbind(s:S1) = println("s1 is gone!")

  def start = println("Comp2 started")

}

object TestDependencies
{
  var s1:Option[S1] = None //injected by Activator
  var s2:Option[S2] = None //injected by Activator
}


import org.scalatest.{FlatSpec, Matchers}

class DMSpec extends FlatSpec with Matchers
{
  import TestDependencies._

  "DM" should "bind all dependencies" in {
    s1.get
    s2.get
  }
}
