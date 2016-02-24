package org.apache.felix.dm.scala

import org.apache.felix.dm.Dependency
import org.apache.felix.dm.DependencyManager

trait ServiceDependencyBuilder[S,C]
{
  def filter(f:String):ServiceDependencyBuilder[S,C]
  def filter(key:String, value:String):ServiceDependencyBuilder[S,C]
  def orFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C]
  def andFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C]

  def added(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C]
  def changed(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C]
  def removed(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C]
  def swapped(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C]
}

object ServiceDependencyBuilder
{
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
      def _added(s:S):Unit = b.o_added foreach (_(comp, s))
      def _changed(s:S):Unit = b.o_changed foreach (_(comp, s))
      def _removed(s:S):Unit = b.o_removed foreach (_(comp, s))
      def _swapped(s:S):Unit = b.o_swapped foreach (_(comp, s))
    }, "_added", "_changed", "_removed", "_swapped")
    d
  }

  private case class ServiceDependencyBuilderImpl[S,C](
    required:Boolean, 
    service:Class[S],
    o_filter:Option[String] = None,
    o_added:Option[(C,S) => Unit] = None,
    o_changed:Option[(C,S) => Unit] = None,
    o_removed:Option[(C,S) => Unit] = None,
    o_swapped:Option[(C,S) => Unit] = None
  )
   extends ServiceDependencyBuilder[S,C]
  {
    def filter(f:String):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(f))
    def filter(key:String, value:String):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"($key=$value)"))
    def orFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"(|${concat(fs.toList)})"))
    def andFilter(fs:(String,String)*):ServiceDependencyBuilder[S,C] = copy(o_filter = Some(s"(&${concat(fs.toList)})"))
    private def concat(fs:List[(String,String)]) = fs.map { case (k,v) => s"($k=$v)" }.mkString

    def added(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C] = copy(o_added = Some(cb))
    def changed(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C] = copy(o_changed = Some(cb))
    def removed(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C] = copy(o_removed = Some(cb))
    def swapped(cb:(C,S) => Unit):ServiceDependencyBuilder[S,C] = copy(o_swapped = Some(cb))
  }
}
