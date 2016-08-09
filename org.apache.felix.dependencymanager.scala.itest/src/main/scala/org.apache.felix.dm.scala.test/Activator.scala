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
      _.provides[S1]("name" -> "comp1")
       .start(_.run)
    }

    component[Comp2] {
      _.provides[S2]()
       .requires[S1] {
         _.filter("name", "comp1")
          .added((inst,s) => inst.bind(s))
       }
       .optionally[S1](_.filter("name", "whatever"))
       .start(_.go)
    }

    // inject services to ourself so we can start the actual test
    component(this) {
      _.requires[S1] {
         _.added((inst,s) => inst.bind(s))
       }
       .requires[S2] {
         _.added((inst,s) => inst.bind(s))
       }
       .init(_.test_init)
       .start(_.start)
    }
  }

  def bind(s:S1) = TestDependencies.s1 = true
  def bind(s:S2) = TestDependencies.s2 = true
  def test_init = TestDependencies.init = true
  def test_stop = TestDependencies.stop = true
  def test_destroy = TestDependencies.destroy = true

  def start:Unit = Future {
    TestDependencies.start = true //obviously :/
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
{
  def run = println("Comp1 started")
}

trait S2
class Comp2 extends S2
{
  private var s1:S1 = _ // injected by callback
  //private var o1:Option[S1] = None // injected by field

  def bind(s:S1) = s1 = s
  def unbind(s:S1) = println("s1 is gone!")

  def go = println("Comp2 started")

}

object TestDependencies
{
  var s1:Boolean = false
  var s2:Boolean = false
  var init:Boolean = false
  var start:Boolean = false
  var stop:Boolean = false
  var destroy:Boolean = false
}


import org.scalatest.{FlatSpec, Matchers}

class DMSpec extends FlatSpec with Matchers
{
  import TestDependencies._

  "DM" should "bind all dependencies" in {
    assert(s1, "S1 not bound")
    assert(s2, "S2 not bound")
  }
  
  it should "call the init and start lifecycle callback methods" in {
    assert(init, "init not called")
    assert(start, "start not called")
  }

  ignore should "call the stop and destroy lifecycle callback methods" in {
    assert(stop, "start not called")
    assert(destroy, "start not called")
  }
}
