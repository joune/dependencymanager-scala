package org.apache.felix.dm.scala.test.launcher

import java.util.HashMap
import java.util.ServiceLoader
import org.osgi.framework.launch.FrameworkFactory
import org.apache.felix.main.AutoProcessor
import collection.JavaConversions._
 
object Launcher
{
  def main(bundles:Array[String]) =
  {
    val factory:FrameworkFactory = ServiceLoader.load(classOf[FrameworkFactory],
      getClass.getClassLoader).iterator.next
    val felixProps = new HashMap[String, String]
    felixProps.put("org.osgi.framework.storage.clean", "onFirstInit")
    felixProps.put("org.osgi.framework.bootdelegation", "sun.*,com.sun.*")
    felixProps.put("gosh.args", "-s") //prevent gogo shell from stopping the framework!
    System.getProperties foreach { case (k,v) => felixProps.put(k, v) }

    val bundleSet = bundles.toSet // filter duplicates
    println("Starting Felix with ")
    bundleSet.foreach(b => println("\t"+new java.io.File(b).getName))
    
    List("install", "start").foreach { a =>
      felixProps.put(s"felix.auto.$a", bundleSet.map("file:"+_).mkString(" "))
    }
    val felix = factory.newFramework(felixProps)
    felix.init
    AutoProcessor.process(felixProps, felix.getBundleContext)
    felix.start
  }
}
