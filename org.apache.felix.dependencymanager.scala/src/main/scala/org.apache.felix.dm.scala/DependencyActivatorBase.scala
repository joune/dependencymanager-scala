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

  implicit def dependencymanager = dm

  def start(bc :BundleContext) :Unit = {
    dm = new DependencyManager(bc)
    init()
  }

  def stop(bc :BundleContext) :Unit = destroy()

  def init():Unit
  def destroy() = Unit

  def component[T](c: T) = ComponentBuilder(c)
  def component[T :TypeTag] = ComponentBuilder[T]
  def component[T](f: () => T) = ComponentBuilder(f)
}

object Helpers
{
  def getClassOf[T : TypeTag] = typeTag[T].mirror.runtimeClass(typeOf[T].typeSymbol.asClass).asInstanceOf[Class[T]]
}
