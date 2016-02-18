package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext

trait DependencyActivatorBase extends BundleActivator
{
  private var dm:DependencyManager = _

  def start(bc :BundleContext) :Unit = {
    dm = new DependencyManager(bc)
    init()
  }

  def stop(bc :BundleContext) :Unit = destroy()

  def init():Unit
  def destroy() = Unit

  def component[T](c: T)(configure :ComponentBuilder[T] => ComponentBuilder[T]) =
    dm.add(ComponentBuilder(dm, c, configure))

  def component[T](c: Class[T])(configure :ComponentBuilder[T] => ComponentBuilder[T]) =
    dm.add(ComponentBuilder(dm, c, configure))

  def component[T](f: () => T)(configure :ComponentBuilder[T] => ComponentBuilder[T]) =
    dm.add(ComponentBuilder(dm, f, configure))
}
