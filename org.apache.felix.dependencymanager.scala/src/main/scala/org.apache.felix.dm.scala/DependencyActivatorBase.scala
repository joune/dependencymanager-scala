package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext

import scala.reflect.runtime.universe._

// if we use a trait instead of an abstract class here, bnd complains 
// that the implementing "Activator does not implement BundleActivator"
abstract class DependencyActivatorBase extends BundleActivator
{
  protected var dm:DependencyManager = _

  def start(bc :BundleContext) :Unit = {
    dm = new DependencyManager(bc)
    init()
  }

  def stop(bc :BundleContext) :Unit = destroy()

  def init():Unit
  def destroy() = Unit

  def component[T](c: T)(configure :ComponentBuilder => ComponentBuilder) =
    dm.add(ComponentBuilder(dm, c, configure))

  def component[T :TypeTag](configure :ComponentBuilder => ComponentBuilder) =
    dm.add(ComponentBuilder(dm, Helpers.getClassOf[T], configure))

  def component[T](f: () => T)(configure :ComponentBuilder => ComponentBuilder) =
    dm.add(ComponentBuilder(dm, f, configure))
}

object Helpers
{
  def getClassOf[T : TypeTag] = typeTag[T].mirror.runtimeClass(typeOf[T].typeSymbol.asClass).asInstanceOf[Class[T]]
}
