package org.apache.felix.dm.scala

import org.apache.felix.dm.Component
import java.util.{Dictionary,Hashtable}

trait ComponentBuilder
{
  def impl[I](i :I) :ComponentBuilder
  def impl[C](c :Class[C]) :ComponentBuilder
  def factory[I](f :() => I) :ComponentBuilder

  def properties(props :(Any,Any)*) :ComponentBuilder

  //def provides(s :Class[_]*) :ComponentBuilder
  def provides(props :Map[_,_], s :Class[_]*) :ComponentBuilder

  def init(f :() => Unit) :ComponentBuilder
  def start(f :() => Unit) :ComponentBuilder
  def stop(f :() => Unit) :ComponentBuilder
  def destroy(f :() => Unit) :ComponentBuilder

  def build :Component
}

object ComponentBuilder
{
  type Factory[T] = () => T
  type LifeCycle = () => Unit
  type Service[T] = (Class[T],Map[_,_])

  def apply() :ComponentBuilder = ComponentBuilderImpl()

  private case class ComponentBuilderImpl(
    impl :Option[Any] = None,
    implClass :Option[Class[_]] = None,
    factory :Option[Factory[_]] = None,
    provides :List[Service[_]] = Nil,
    properties :Option[Dictionary[_,_]] = None,
    init :Option[LifeCycle] = None,
    start :Option[LifeCycle] = None,
    stop :Option[LifeCycle] = None,
    destroy :Option[LifeCycle] = None,
    dependencies :List[DependencyBuilder] = Nil
  ) 
  extends ComponentBuilder 
  {
    def impl[I](i :I) :ComponentBuilder = copy(impl = Some(i))
    def impl[C](c :Class[C]) :ComponentBuilder = copy(implClass = Some(c))
    def factory[I](f :() => I) :ComponentBuilder = copy(factory = Some(f))

    def properties(props :(Any,Any)*) :ComponentBuilder = {
      val dic = new Hashtable[Any,Any]() //OSGi internally requires a Dictionary :/
      props.foreach { case(k,v) => dic.put(k,v) }
      copy(properties = Some(dic))
    }

    //def provides(s :Class[_]*) :ComponentBuilder = provides(Map(), s)
    def provides(props :Map[_,_], s :Class[_]*) :ComponentBuilder = 
      copy(provides = provides ++ (s.toList.map((_,props))))

    def init(f :() => Unit) :ComponentBuilder = copy(init = Some(f))
    def start(f :() => Unit) :ComponentBuilder = copy(init = Some(f))
    def stop(f :() => Unit) :ComponentBuilder = copy(init = Some(f))
    def destroy(f :() => Unit) :ComponentBuilder = copy(init = Some(f))

    def build :Component = ???
  }
}
