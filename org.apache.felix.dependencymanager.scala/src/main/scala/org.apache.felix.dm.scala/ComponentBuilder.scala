package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import java.util.{Dictionary,Hashtable}

import scala.reflect.runtime.universe._

trait ComponentBuilder
{
  type C

  def provides[S >: C](props: (String,Any)*)(implicit tt:TypeTag[S]): ComponentBuilder

  def init(f: C => Unit): ComponentBuilder
  def start(f: C => Unit): ComponentBuilder
  def stop(f: C => Unit): ComponentBuilder
  def destroy(f: C => Unit): ComponentBuilder

  def requires[D](configure: ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C])(implicit tt:TypeTag[D]): ComponentBuilder
  def optionally[D](configure: ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C])(implicit tt:TypeTag[D]): ComponentBuilder
}

object ComponentBuilder
{
  type Factory[C] = () => C
  type LifeCycle[C] = C => Unit
  type CompConfig = ComponentBuilder => ComponentBuilder
  type DepsConfig[D,C] = ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C]

  def apply[C](dm: DependencyManager, c: C, configure: CompConfig) =
    build(dm, FromImpl(c), configure)

  def apply[C](dm: DependencyManager, c: Class[C], configure: CompConfig) =
    build(dm, FromClass(c), configure)

  def apply[C](dm: DependencyManager, f: () => C, configure: CompConfig) =
    build(dm, FromFactory(f), configure)

  private def build[C](dm:DependencyManager, b:ComponentBuilderImpl[C], configure:CompConfig): Component = {
    val comp = dm.createComponent
    b match {
      case FromImpl(i) => comp.setImplementation(i)
      case FromClass(c) => comp.setImplementation(c)
      case FromFactory(f) => ??? //FIXME
    }
    b.provides foreach { (clazz:Class[_]) => 
      comp.setInterface(clazz.getName, null)
    }
    b.properties foreach ( comp.setServiceProperties(_) )
    comp.setCallbacks(new Object {
      lazy val inst = comp.getInstance.asInstanceOf[C]
      def _init():Unit = b.init foreach (_(inst))
      def _start():Unit = b.start foreach (_(inst))
      def _stop():Unit = b.stop foreach (_(inst))
      def _destroy():Unit = b.destroy foreach (_(inst))
    }, "_init", "_start", "_stop", "_destroy")
    b.dependencies foreach { d => 
      comp.add(ServiceDependencyBuilder.build(dm, d, () => comp.getInstance.asInstanceOf[C]))
    }
    comp
  }

  sealed private abstract class ComponentBuilderImpl[T](
    var provides: Option[Class[_]] = None,
    var properties: Option[Dictionary[String,_]] = None,
    var init: Option[LifeCycle[T]] = None,
    var start: Option[LifeCycle[T]] = None,
    var stop: Option[LifeCycle[T]] = None,
    var destroy: Option[LifeCycle[T]] = None,
    var dependencies: List[ServiceDependencyBuilder[_,T]] = Nil
  )
  extends ComponentBuilder
  {
    type C = T

    def provides[S >: C](props: (String,Any)*)(implicit tt:TypeTag[S]): ComponentBuilder = {
      val dic = new Hashtable[String,Any]() //OSGi internally requires a Dictionary: /
      props.foreach { case(k,v) => dic.put(k,v) }
      provides = Some(Helpers.getClassOf[S])
      properties = Some(dic)
      this
    }

    def init(f: C => Unit): ComponentBuilder = {
      init = Some(f)
      this
    }
    def start(f: C => Unit): ComponentBuilder = {
      start = Some(f)
      this
    }
    def stop(f: C => Unit): ComponentBuilder = {
      stop = Some(f)
      this
    }
    def destroy(f: C => Unit): ComponentBuilder = {
      destroy = Some(f)
      this
    }

    def requires[D](configure: DepsConfig[D,C])(implicit tt:TypeTag[D]): ComponentBuilder = 
      addDependency(true, configure)
    def optionally[D](configure: DepsConfig[D,C])(implicit tt:TypeTag[D]): ComponentBuilder = 
      addDependency(false, configure)

    def addDependency[D](required:Boolean, configure: DepsConfig[D,C])(implicit tt:TypeTag[D]): ComponentBuilder = {
      dependencies = configure(ServiceDependencyBuilder(required, Helpers.getClassOf[D]))::dependencies
      this
    }
  }

  private case class FromImpl[T](impl: T) extends ComponentBuilderImpl[T]
  private case class FromClass[T](implClass: Class[T]) extends ComponentBuilderImpl[T]
  private case class FromFactory[T](factory: Factory[T]) extends ComponentBuilderImpl[T]

}
