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

  def register(dm:DependencyManager): Component
}

object ComponentBuilder
{
  type Factory[C] = () => C
  type LifeCycle[C] = C => Unit
  type DepsConfig[D,C] = ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C]

  def apply[T](c: T): ComponentBuilder = FromImpl(c)
  def apply[T :TypeTag]: ComponentBuilder = FromClass(Helpers.getClassOf[T])
  def apply[T](f: () => T): ComponentBuilder = FromFactory(f)

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

    def setImplementation(comp:Component): Unit

    def register(dm:DependencyManager): Component = {
      val comp = dm.createComponent
      setImplementation(comp)
      provides foreach { (clazz:Class[_]) => 
        comp.setInterface(clazz.getName, null)
      }
      properties foreach ( comp.setServiceProperties(_) )
      comp.setCallbacks(new Object {
        lazy val inst = comp.getInstance.asInstanceOf[C]
        def _init():Unit = init foreach (_(inst))
        def _start():Unit = start foreach (_(inst))
        def _stop():Unit = stop foreach (_(inst))
        def _destroy():Unit = destroy foreach (_(inst))
      }, "_init", "_start", "_stop", "_destroy")
      dependencies foreach { d => 
        comp.add(ServiceDependencyBuilder.build(dm, d, () => comp.getInstance.asInstanceOf[C]))
      }
      comp
    }

  }

  private case class FromImpl[T](impl: T) extends ComponentBuilderImpl[T] {
    def setImplementation(comp:Component) = comp.setImplementation(impl)
  }
  private case class FromClass[T](implClass: Class[T]) extends ComponentBuilderImpl[T] {
    def setImplementation(comp:Component) = comp.setImplementation(implClass)
  }
  private case class FromFactory[T](factory: Factory[T]) extends ComponentBuilderImpl[T] {
    def setImplementation(comp:Component) = ??? //FIXME (setFactory..)
  }

}
