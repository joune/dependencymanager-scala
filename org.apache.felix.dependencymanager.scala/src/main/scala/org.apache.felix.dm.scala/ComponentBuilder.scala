package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import java.util.{Dictionary,Hashtable}

import scala.reflect.runtime.universe._

trait ComponentBuilder[C]
{
  def provides[S >: C](props: (String,Any)*)(implicit tt:TypeTag[S]): ComponentBuilder[C]

  def init(f: C => Unit): ComponentBuilder[C]
  def start(f: C => Unit): ComponentBuilder[C]
  def stop(f: C => Unit): ComponentBuilder[C]
  def destroy(f: C => Unit): ComponentBuilder[C]

  def addDependency[D](required:Boolean, configure: ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C], tt: TypeTag[D]): ComponentBuilder[C]

  def requires[D: TypeTag] = new ServiceDependencyConfig(this, true, typeTag[D])
  def optionally[D: TypeTag] = new ServiceDependencyConfig(this, false, typeTag[D])

  def register(implicit dm:DependencyManager): Component
}

// this class serves as a method indirection to "curry" the TypeTag and 'configure' implicit parameters, to enable default configurations
// FIXME: I'm not too happy with the "withConf" wording of the API, especially when using the implicit default value :/
class ServiceDependencyConfig[C,D](comp: ComponentBuilder[C], required: Boolean, tt:TypeTag[D]) {
  def withConf(implicit configure: ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C]): ComponentBuilder[C] =
    comp.addDependency[D](required, configure, tt)
}

object ComponentBuilder
{
  type Factory[C] = () => C
  type LifeCycle[C] = C => Unit
  type DepsConfig[D,C] = ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C]

  def apply[C](c: C)(implicit dm:DependencyManager): ComponentBuilder[C] = FromImpl(c)
  def apply[C :TypeTag](implicit dm:DependencyManager): ComponentBuilder[C] = FromType[C]
  def apply[C](f: () => C)(implicit dm:DependencyManager): ComponentBuilder[C] = FromFactory(f)

  sealed private abstract class ComponentBuilderImpl[C](
    var provides: Option[Class[_]] = None,
    var properties: Option[Dictionary[String,_]] = None,
    var init: Option[LifeCycle[C]] = None,
    var start: Option[LifeCycle[C]] = None,
    var stop: Option[LifeCycle[C]] = None,
    var destroy: Option[LifeCycle[C]] = None,
    var dependencies: List[ServiceDependencyBuilder[_,C]] = Nil
  )
  extends ComponentBuilder[C]
  {
    def provides[S >: C](props: (String,Any)*)(implicit tt:TypeTag[S]): ComponentBuilder[C] = {
      val dic = new Hashtable[String,Any]() //OSGi internally requires a Dictionary: /
      props.foreach { case(k,v) => dic.put(k,v) }
      provides = Some(Helpers.getClassOf[S])
      properties = Some(dic)
      this
    }

    def init(f: C => Unit): ComponentBuilder[C] = {
      init = Some(f)
      this
    }
    def start(f: C => Unit): ComponentBuilder[C] = {
      start = Some(f)
      this
    }
    def stop(f: C => Unit): ComponentBuilder[C] = {
      stop = Some(f)
      this
    }
    def destroy(f: C => Unit): ComponentBuilder[C] = {
      destroy = Some(f)
      this
    }

    def addDependency[D](required:Boolean, configure: ServiceDependencyBuilder[D,C] => ServiceDependencyBuilder[D,C], tt: TypeTag[D]): ComponentBuilder[C] = {
      dependencies = configure(ServiceDependencyBuilder(required, Helpers.getClassOf[D](tt)))::dependencies
      this
    }

    def setImplementation(comp:Component): Unit

    def register(implicit dm:DependencyManager): Component = {
      val comp = buildComp(dm)
      dm.add(comp)
      comp
    }
    private def buildComp(dm:DependencyManager): Component = {
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

  private case class FromImpl[C](impl: C)(implicit dm:DependencyManager) extends ComponentBuilderImpl[C] {
    def setImplementation(comp:Component) = comp.setImplementation(impl)
  }
  private case class FromType[C](implicit tt: TypeTag[C], dm:DependencyManager) extends ComponentBuilderImpl[C] {
    def setImplementation(comp:Component) = comp.setImplementation(Helpers.getClassOf[C])
  }
  private case class FromFactory[C](factory: Factory[C])(implicit dm:DependencyManager) extends ComponentBuilderImpl[C] {
    def setImplementation(comp:Component) = ??? //FIXME (setFactory..)
  }

}
