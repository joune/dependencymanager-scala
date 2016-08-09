package org.apache.felix.dm.scala.test

// 'hack'?.. we must hide Predef.$conforms in order to enable autoInjectService
// see http://stackoverflow.com/questions/5377492/problem-with-implicit-ambiguity-between-my-method-and-conforms-in-predef#5379062
import Predef.{$conforms => _, _}
import org.apache.felix.dm.scala.Implicits.autoInjectService

import org.apache.felix.dm.scala.{DependencyActivatorBase, ServiceDependencyBuilder}

import org.scalatest.tools.Runner
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

import ServiceDependencyBuilder.Props //just an alias for Map[String,Object]


class Activator extends DependencyActivatorBase
{
  def init() = {
    // initialize some random components
    component(new Comp1())
      .provides[S1]("name" -> "comp1")
      .start(_.run)
      .register

    component[Comp2]
      .provides[S2]()
      .requires[S1] {
        _.filter("name", "comp1")
         .added(i => i.bind _)
      }
      .optionally[S1](_.filter("name", "whatever"))
      .start(_.go)
      .register


    // inject services to ourself so we can start the actual test
    component(this)
      .requires[S1] {
        _.added(i => i.bind _)
      }
      .requires[S2] // plugged default behaviour = autoInjectService
      .init(_.test_init)
      .start(_.start)
      .register
  }

  var s2:S2 = _ //injected
  def bind(s:S1,p:Props) = Tests.s1 = s != null && p("name") == "comp1"
  def test_init = Tests.init = true
  def test_stop = Tests.stop = true
  def test_destroy = Tests.destroy = true

  def start:Unit = Future {
    Tests.start = true //obviously :/
    Tests.s2 = s2 != null //check that s2 was injected

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

  def bind(s:S1, props:Props) = s1 = s
  def unbind(s:S1) = println("s1 is gone!")

  def go = println("Comp2 started")

}

object Tests
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
  import Tests._

  "DM" should "bind callback dependencies" in {
    assert(s1, "S1 not bound")
  }

  it should "bind injected dependencies" in {
    assert(s2, "S2 not bound")
  }
  
  it should "call the init and start lifecycle callback methods" in {
    assert(init, "init not called")
    assert(start, "start not called")
  }

  ignore should "call the stop and destroy lifecycle callback methods" in {
    assert(stop, "stop not called")
    assert(destroy, "destroy not called")
  }
}
