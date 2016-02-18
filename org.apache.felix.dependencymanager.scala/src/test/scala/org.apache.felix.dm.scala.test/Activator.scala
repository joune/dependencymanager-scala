package org.apache.felix.dm.scala.test

import org.apache.felix.dm.scala.DependencyActivatorBase

class Activator extends DependencyActivatorBase
{
  def init() = {
    component(new Comp1()) {
      _.provides(classOf[S1], "name" -> "comp1")
       .start(_.start)
    }
    component(classOf[Comp2]) {
      _.provides(classOf[S2])
       .requires(classOf[S1])(_.filter("name", "comp1"))
       //.optionally(classOf[S1])(_.filter("name", "whatever"))
       .start(_.start)
    }
  }
}

trait S1
class Comp1 extends S1
{
  def start = println("Comp1 started")
}

trait S2
class Comp2 extends S2
{
  private var s1:S1 = _ // injected by callback
  //private var o1:Option[S1] = None // injected by field

  def bind(s:S1) = s1 = s
  def unbind(s:S1) = println("s1 is gone!")

  def start = println("Comp2 started")

}
