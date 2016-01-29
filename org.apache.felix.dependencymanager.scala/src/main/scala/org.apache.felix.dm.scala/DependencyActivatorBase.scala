package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import org.osgi.framework.BundleActivator
import org.osgi.framework.BundleContext

trait DependencyActivatorBase extends BundleActivator
{
  var dm:DependencyManager = _

  def start(bc :BundleContext) :Unit = {
    dm = new DependencyManager(bc)
    init()
  }

  def stop(bc :BundleContext) :Unit = destroy()

  def init():Unit
  def destroy():Unit

  def component(configure :ComponentBuilder => ComponentBuilder) = 
    dm.add(ComponentBuilder.build(dm, configure(ComponentBuilder())))
}
