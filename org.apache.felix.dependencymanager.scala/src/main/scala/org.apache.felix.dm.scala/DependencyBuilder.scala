package org.apache.felix.dm.scala

import org.apache.felix.dm.Dependency
import org.apache.felix.dm.DependencyManager

trait DependencyBuilder[S,C]
{
  def filter(f:String):DependencyBuilder[S,C]
  def filter(key:String, value:String):DependencyBuilder[S,C]
  def orFilter(fs:(String,String)*):DependencyBuilder[S,C]
  def andFilter(fs:(String,String)*):DependencyBuilder[S,C]

  def added(cb:(C,S) => Unit):DependencyBuilder[S,C]
  def changed(cb:(C,S) => Unit):DependencyBuilder[S,C]
  def removed(cb:(C,S) => Unit):DependencyBuilder[S,C]
  def swapped(cb:(C,S) => Unit):DependencyBuilder[S,C]
}

object DependencyBuilder
{
  def apply[S,C](required:Boolean, s:Class[S]) :DependencyBuilder[S,C] = {
    DependencyBuilderImpl(required, s)
  }

  def build[S,C](dm:DependencyManager, builder:DependencyBuilder[S,C], inst : () => C) :Dependency = {
    val d = dm.createServiceDependency
    val b = builder.asInstanceOf[DependencyBuilderImpl[S,C]]
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

  private case class DependencyBuilderImpl[S,C](
    required:Boolean, 
    service:Class[S],
    o_filter:Option[String] = None,
    o_added:Option[(C,S) => Unit] = None,
    o_changed:Option[(C,S) => Unit] = None,
    o_removed:Option[(C,S) => Unit] = None,
    o_swapped:Option[(C,S) => Unit] = None
  )
   extends DependencyBuilder[S,C]
  {
    def filter(f:String):DependencyBuilder[S,C] = copy(o_filter = Some(f))
    def filter(key:String, value:String):DependencyBuilder[S,C] = copy(o_filter = Some(s"($key=$value)"))
    def orFilter(fs:(String,String)*):DependencyBuilder[S,C] = copy(o_filter = Some(s"(|${concat(fs.toList)})"))
    def andFilter(fs:(String,String)*):DependencyBuilder[S,C] = copy(o_filter = Some(s"(&${concat(fs.toList)})"))
    private def concat(fs:List[(String,String)]) = fs.map { case (k,v) => s"($k=$v)" }.mkString

    def added(cb:(C,S) => Unit):DependencyBuilder[S,C] = copy(o_added = Some(cb))
    def changed(cb:(C,S) => Unit):DependencyBuilder[S,C] = copy(o_changed = Some(cb))
    def removed(cb:(C,S) => Unit):DependencyBuilder[S,C] = copy(o_removed = Some(cb))
    def swapped(cb:(C,S) => Unit):DependencyBuilder[S,C] = copy(o_swapped = Some(cb))
  }
}
