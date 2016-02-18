package org.apache.felix.dm.scala

trait DependencyBuilder[S]
{
  def filter(f:String):DependencyBuilder[S]
  def filter(key:String, value:String):DependencyBuilder[S]
  def orFilter(fs:(String,String)*):DependencyBuilder[S]
  def andFilter(fs:(String,String)*):DependencyBuilder[S]
}

object DependencyBuilder
{
  def apply[S](required:Boolean, s:Class[S]) :DependencyBuilder[S] = DependencyBuilderImpl(required, s)

  private case class DependencyBuilderImpl[S](
    required:Boolean, 
    service:Class[S],
    filter:Option[String] = None
  )
   extends DependencyBuilder[S]
  {
    def filter(f:String):DependencyBuilder[S] = copy(filter = Some(f))
    def filter(key:String, value:String):DependencyBuilder[S] = copy(filter = Some(s"($key=$value)"))
    def orFilter(fs:(String,String)*):DependencyBuilder[S] = copy(filter = Some(s"(|${concat(fs.toList)})"))
    def andFilter(fs:(String,String)*):DependencyBuilder[S] = copy(filter = Some(s"(&${concat(fs.toList)})"))
    private def concat(fs:List[(String,String)]) = fs.map { case (k,v) => s"($k=$v)" }.mkString
  }
}
