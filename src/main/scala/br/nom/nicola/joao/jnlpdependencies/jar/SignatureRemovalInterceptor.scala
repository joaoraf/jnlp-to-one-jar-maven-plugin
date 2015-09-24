package br.nom.nicola.joao.jnlpdependencies.jar

import java.util.zip.ZipEntry
import java.io.ByteArrayInputStream
import java.util.jar.Manifest
import java.io.ByteArrayOutputStream
import scala.collection.immutable.TreeMap
import java.util.jar.Attributes

object CaseInsensitiveStringOrdering extends Ordering[String] {
    def compare(x: String, y: String) = x.compareToIgnoreCase(y)
}

case class ScalaManifest(
    mainAttributes : Map[String,String] = new TreeMap()(CaseInsensitiveStringOrdering),
    entries : Map[String,Map[String,String]] = new TreeMap()(CaseInsensitiveStringOrdering).withDefaultValue(new TreeMap()(CaseInsensitiveStringOrdering))
)  {
  def toManifest : Manifest = {
    val m = new Manifest()
    val mMainAttrs = m.getMainAttributes
    mainAttributes foreach { case (k,v) => mMainAttrs.putValue(k,v) }
    val mEntries = m.getEntries
    entries foreach { 
      case(e,m) => 
        val a = new Attributes()
        m foreach { case (k,v) => a.putValue(k,v) }
        mEntries.put(e,a)
    }
    m
  }
}
    
object ScalaManifest {
  private implicit class ToMap(a : Any) {
    def toCaseInsensitiveMap : Map[String,Any] = a match {
      case m : java.util.Map[_,_] =>
        import scala.collection.JavaConversions._  
        new TreeMap()(CaseInsensitiveStringOrdering) ++ (m.to[Seq].map({case (k,v) => (k.toString.toLowerCase,v)}))
      case _ => sys.error("toCaseInsensitiveMap")
    }
  }
 
  def apply(m : Manifest) : ScalaManifest = {      
      val mainAttrs = m.getMainAttributes.toCaseInsensitiveMap.mapValues(_.asInstanceOf[String])
      val entries : Map[String,Map[String,String]] = 
        m.getEntries.toCaseInsensitiveMap.mapValues(
            _.toCaseInsensitiveMap.mapValues(_.asInstanceOf[String]))
      ScalaManifest(mainAttrs,entries)
  }
  
}

object SignatureRemovalDecider extends Interceptor {
 
  val emptyCIMap : Map[String,Nothing] = new TreeMap()(CaseInsensitiveStringOrdering)
  
  val sigRE = """^META-INF/[^/]*.(SF|RSA)$""".r
  
  trait ManifestFilter extends InterceptorFilter {    
    
    final override def data(d: Array[Byte]): Array[Byte] = {
     
      val m = new Manifest(new ByteArrayInputStream(d))
      
      val m1 = process(ScalaManifest(m)).toManifest
      val bos = new ByteArrayOutputStream()
      m1.write(bos)
      val res = bos.toByteArray()
      System.err.println(s"ManifestFilter: in = ${new String(d)}, out = ${new String(res)}")
      res
    }
    def process(m : ScalaManifest) : ScalaManifest
  }
  
  object SignatureManifestFilter extends ManifestFilter {
    override def process(m : ScalaManifest) = {
      m copy (mainAttributes = m.mainAttributes -- Seq("Codebase","Sealed"), entries = emptyCIMap)           
    }
  }
  
  override def decide(ze: ZipEntry): InterceptorDecision = ze.getName match {
    case sigRE(_) => SkipEntry
    case "META-INF/MANIFEST.MF" => InterceptEntry(SignatureManifestFilter)
    case _ => PasstroughEntry
  }

  
  
}

