package org.apache.felix.dm.scala

import org.apache.felix.dm.Dependency
import org.apache.felix.dm.DependencyManager

trait DependencyBuilder[S]
{
  def filter(f:String):DependencyBuilder[S]
  def filter(key:String, value:String):DependencyBuilder[S]
  def orFilter(fs:(String,String)*):DependencyBuilder[S]
  def andFilter(fs:(String,String)*):DependencyBuilder[S]
}

object DependencyBuilder
{
  def apply[S](required:Boolean, s:Class[S]) :DependencyBuilder[S] = {
    DependencyBuilderImpl(required, s)
  }

  def build[S](dm:DependencyManager, builder:DependencyBuilder[S]) :Dependency = {
    val d = dm.createServiceDependency
    val b = builder.asInstanceOf[DependencyBuilderImpl[S]]
    b.o_filter match {
      case Some(f) => d.setService(b.service, f)
      case None    => d.setService(b.service)
    }
    d
  }

  private case class DependencyBuilderImpl[S](
    required:Boolean, 
    service:Class[S],
    o_filter:Option[String] = None
  )
   extends DependencyBuilder[S]
  {
    def filter(f:String):DependencyBuilder[S] = copy(o_filter = Some(f))
    def filter(key:String, value:String):DependencyBuilder[S] = copy(o_filter = Some(s"($key=$value)"))
    def orFilter(fs:(String,String)*):DependencyBuilder[S] = copy(o_filter = Some(s"(|${concat(fs.toList)})"))
    def andFilter(fs:(String,String)*):DependencyBuilder[S] = copy(o_filter = Some(s"(&${concat(fs.toList)})"))
    private def concat(fs:List[(String,String)]) = fs.map { case (k,v) => s"($k=$v)" }.mkString
  }
}
