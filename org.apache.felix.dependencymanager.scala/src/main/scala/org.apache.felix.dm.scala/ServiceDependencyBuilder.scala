package org.apache.felix.dm.scala

import org.apache.felix.dm.Dependency
import org.apache.felix.dm.DependencyManager
import ServiceDependencyBuilder.Props

trait ServiceDependencyBuilder[S,C]
{
  import ServiceDependencyBuilder.Callback

  def filter(f:String):ServiceDependencyBuilder[S,C]
  def filter(key:String, value:String):ServiceDependencyBuilder[S,C]
  def orFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C]
  def andFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C]

  def added  (cb:Callback[C,S]):ServiceDependencyBuilder[S,C]
  def changed(cb:Callback[C,S]):ServiceDependencyBuilder[S,C]
  def removed(cb:Callback[C,S]):ServiceDependencyBuilder[S,C]
  def swapped(cb:Callback[C,S]):ServiceDependencyBuilder[S,C]
}

object ServiceDependencyBuilder
{
  import java.util.{Map => jMap}

  type Props = Map[String,Object]
  type jProps = jMap[String,Object]
  type Callback[C,S] = C => (S,Props) => Unit

  implicit def toProps(p:jProps) :Props = scala.collection.JavaConversions.mapAsScalaMap(p).toMap

  def apply[S,C](required:Boolean, s:Class[S]) :ServiceDependencyBuilder[S,C] = {
    ServiceDependencyBuilderImpl(required, s)
  }

  def build[S,C](dm:DependencyManager, builder:ServiceDependencyBuilder[S,C], inst : () => C) :Dependency = {
    val d = dm.createServiceDependency
    val b = builder.asInstanceOf[ServiceDependencyBuilderImpl[S,C]]
    d.setRequired(b.required)
    b.o_filter match {
      case Some(f) => d.setService(b.service, f)
      case None    => d.setService(b.service)
    }
    d.setCallbacks(new Object {
      lazy val comp = inst()
      //FIXME callback not found !? def _added  (s:S, p:jProps):Unit = b.o_added   foreach (_(comp)(s, p))
      //FIXME callback not found !? def _changed(s:S, p:jProps):Unit = b.o_changed foreach (_(comp)(s, p))
      //FIXME callback not found !? def _removed(s:S, p:jProps):Unit = b.o_removed foreach (_(comp)(s, p))
      //FIXME callback not found !? def _swapped(s:S, p:jProps):Unit = b.o_swapped foreach (_(comp)(s, p))
      def _added  (s:S):Unit = b.o_added   foreach (_(comp)(s, Map[String,Object]()))//FIXME temp workaround, no props
      def _changed(s:S):Unit = b.o_changed foreach (_(comp)(s, Map[String,Object]()))//FIXME temp workaround, no props
      def _removed(s:S):Unit = b.o_removed foreach (_(comp)(s, Map[String,Object]()))//FIXME temp workaround, no props
      def _swapped(s:S):Unit = b.o_swapped foreach (_(comp)(s, Map[String,Object]()))//FIXME temp workaround, no props
    }, "_added", "_changed", "_removed", "_swapped")
    d
  }

  private case class ServiceDependencyBuilderImpl[S,C](
    required:Boolean, 
    service:Class[S],
    o_filter:Option[String] = None,
    o_added  :Option[Callback[C,S]] = None,
    o_changed:Option[Callback[C,S]] = None,
    o_removed:Option[Callback[C,S]] = None,
    o_swapped:Option[Callback[C,S]] = None
  )
   extends ServiceDependencyBuilder[S,C]
  {
    def filter(f:String):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(f))
    def filter(key:String, value:String):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"($key=$value)"))
    def orFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"(|${concat(fs.toList)})"))
    def andFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"(&${concat(fs.toList)})"))
    private def concat(fs:List[(String,String)]) = fs.map { case (k,v) => s"($k=$v)" }.mkString

    def added  (cb:Callback[C,S]):ServiceDependencyBuilder[S,C] = copy(o_added   = Some(cb))
    def changed(cb:Callback[C,S]):ServiceDependencyBuilder[S,C] = copy(o_changed = Some(cb))
    def removed(cb:Callback[C,S]):ServiceDependencyBuilder[S,C] = copy(o_removed = Some(cb))
    def swapped(cb:Callback[C,S]):ServiceDependencyBuilder[S,C] = copy(o_swapped = Some(cb))
  }
}
