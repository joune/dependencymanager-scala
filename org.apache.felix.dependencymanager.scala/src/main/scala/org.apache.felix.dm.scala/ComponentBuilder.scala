package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import org.apache.felix.dm.DependencyManager
import java.util.{Dictionary,Hashtable}

trait ComponentBuilder[T]
{
  def provides[S >: T](s :Class[S], props :(String,Any)*) :ComponentBuilder[T]

  def init(f :T => Unit) :ComponentBuilder[T]
  def start(f :T => Unit) :ComponentBuilder[T]
  def stop(f :T => Unit) :ComponentBuilder[T]
  def destroy(f :T => Unit) :ComponentBuilder[T]

  def requires[D](d :Class[D])(configure :DependencyBuilder[D] => DependencyBuilder[D]) :ComponentBuilder[T]
  def optionally[D](d :Class[D])(configure :DependencyBuilder[D] => DependencyBuilder[D]) :ComponentBuilder[T]
}

object ComponentBuilder
{
  type Factory[T] = () => T
  type LifeCycle[T] = T => Unit
  type CompConfig[T] = ComponentBuilder[T] => ComponentBuilder[T]
  type DepsConfig[T] = DependencyBuilder[T] => DependencyBuilder[T]

  def apply[T](dm :DependencyManager, c: T, configure :CompConfig[T]) =
    build(dm, ComponentBuilderImpl(impl = Some(c)), configure)

  def apply[T](dm :DependencyManager, c: Class[T], configure :CompConfig[T]) =
    build(dm, ComponentBuilderImpl(implClass = Some(c)), configure)

  def apply[T](dm :DependencyManager, f: () => T, configure :CompConfig[T]) =
    build(dm, ComponentBuilderImpl(factory = Some(f)), configure)

  private def build[T](dm:DependencyManager, builder:ComponentBuilderImpl[T], configure:CompConfig[T]) :Component = {
    val b = configure(builder).asInstanceOf[ComponentBuilderImpl[T]]
    val comp = dm.createComponent
    b.impl foreach ( comp.setImplementation(_) )
    b.implClass foreach ( comp.setImplementation(_) )
    //b.factory...
    b.properties foreach ( comp.setServiceProperties(_) )
    comp.setCallbacks(new Object {
      lazy val inst = comp.getInstance.asInstanceOf[T]
      def _init():Unit = b.init foreach (_(inst))
      def _start():Unit = b.start foreach (_(inst))
      def _stop():Unit = b.stop foreach (_(inst))
      def _destroy():Unit = b.destroy foreach (_(inst))
    }, "_init", "_start", "_stop", "_destroy")
    b.dependencies foreach { d => comp.add(DependencyBuilder.build(dm, d)) }
    comp
  }

  private case class ComponentBuilderImpl[T](
    impl :Option[T] = None,
    implClass :Option[Class[T]] = None,
    factory :Option[Factory[T]] = None,
    provides :Option[Class[_]] = None,
    properties :Option[Dictionary[String,_]] = None,
    init :Option[LifeCycle[T]] = None,
    start :Option[LifeCycle[T]] = None,
    stop :Option[LifeCycle[T]] = None,
    destroy :Option[LifeCycle[T]] = None,
    dependencies :List[DependencyBuilder[_]] = Nil
  ) 
  extends ComponentBuilder[T]
  {
    def provides[S >: T](s: Class[S], props :(String,Any)*) :ComponentBuilder[T] = {
      val dic = new Hashtable[String,Any]() //OSGi internally requires a Dictionary :/
      props.foreach { case(k,v) => dic.put(k,v) }
      copy(provides = Some(s), properties = Some(dic))
    }

    def init(f :T => Unit) :ComponentBuilder[T] = copy(init = Some(f))
    def start(f :T => Unit) :ComponentBuilder[T] = copy(init = Some(f))
    def stop(f :T => Unit) :ComponentBuilder[T] = copy(init = Some(f))
    def destroy(f :T => Unit) :ComponentBuilder[T] = copy(init = Some(f))

    def requires[D](d :Class[D])(configure :DepsConfig[D]) :ComponentBuilder[T] = 
      addDependency(true, d, configure)
    def optionally[D](d :Class[D])(configure :DepsConfig[D]) :ComponentBuilder[T] = 
      addDependency(false, d, configure)

    def addDependency[D](required:Boolean, d: Class[D], configure :DepsConfig[D]) :ComponentBuilder[T] =
      copy(dependencies = configure(DependencyBuilder(required, d))::dependencies)
  }
}
