package br.nom.nicola.joao.jnlpdependencies.jar

import java.util.jar.JarOutputStream
import java.util.zip.ZipEntry
import java.io.ByteArrayOutputStream
import java.io.OutputStream

abstract sealed class InterceptorDecision extends Product

case object SkipEntry extends InterceptorDecision

case object PasstroughEntry extends InterceptorDecision

case class InterceptEntry(filter: InterceptorFilter) extends InterceptorDecision

trait InterceptorFilter {
  def zipEntry(ze: ZipEntry): ZipEntry = ze
  def data(d: Array[Byte]): Array[Byte] = d
}

trait Interceptor {
  def decide(ze: ZipEntry): InterceptorDecision
}

class InterceptingJarOutputStream[Accum](decider: Interceptor, os: OutputStream) extends JarOutputStream(os) {
  private var skipping = false
  private var ofilter: Option[InterceptorFilter] = None
  private var interceptStream: Option[ByteArrayOutputStream] = None
  override def putNextEntry(ze: ZipEntry) = {
    finishLastIntercept
    skipping = false
    decider.decide(ze) match {
      case SkipEntry =>
        System.err.println(s"##### skipping ${ze.getName}")
        skipping = true
      case PasstroughEntry =>
        System.err.println(s"##### passing through ${ze.getName}")
        super.putNextEntry(ze)
      case InterceptEntry(filter) =>
        val newZe = filter.zipEntry(ze)
        ofilter = Some(filter)
        interceptStream = Some(new ByteArrayOutputStream(ze.getSize.toInt))        
        super.putNextEntry(newZe)
        System.err.println(s"##### filtering ${ze.getName} => ${newZe.getName}")        
    }
  }
  private def finishLastIntercept() = interceptStream foreach { s =>
    System.err.println("##### finishLastIntercept: have work to do")
    val data = s.toByteArray
    ofilter.foreach {f => 
      val newData = f.data(data)
      System.err.println(s"Writing ${newData.length} (was ${data.length})")
      super.write(newData,0,newData.length)
      super.closeEntry()
    }
    interceptStream = None
    ofilter = None
  }

  override def write(arr: Array[Byte], off: Int, len: Int) =
    interceptStream match {
      case Some(s) => s.write(arr, off, len)
      case None if !skipping => super.write(arr, off, len)
      case _ => ()
    }
  override def write(arr: Array[Byte]) =
    interceptStream match {
      case Some(s) => s.write(arr)
      case None if !skipping => super.write(arr)
      case _ => ()
    }
  override def write(c: Int) =
    interceptStream match {
      case Some(s) => s.write(c)
      case None if !skipping => super.write(c)
      case _ => ()
    }
  override def flush() = {
      System.err.println("##### flush()")
  }
  override def close() = {
    System.err.println("##### close()")
    finishLastIntercept()
    super.close()
  }
  
  override def closeEntry() = {
    System.err.println(s"##### closeEntry: skipping=${skipping}")
    if(!skipping) { 
      finishLastIntercept()
      super.closeEntry()
    }    
  }
  
  override def finish() = {
    System.err.println(s"##### finish")
    finishLastIntercept()
    super.finish()
  }

}
