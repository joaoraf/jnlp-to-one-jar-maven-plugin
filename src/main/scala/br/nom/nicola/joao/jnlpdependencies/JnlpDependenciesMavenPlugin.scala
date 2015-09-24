package br.nom.nicola.joao.jnlpdependencies

import java.io.File

import java.net.URL
import java.util.jar.JarOutputStream
import java.util.jar.Pack200
import java.util.zip.GZIPInputStream
import java.util.zip.ZipEntry
import scala.collection.JavaConversions
import scala.xml.XML
import org.apache.commons.io.FileUtils
import org.apache.maven.execution.MavenSession
import org.apache.maven.plugin.AbstractMojo
import org.apache.maven.plugin.BuildPluginManager
import org.apache.maven.plugins.annotations.Component
import org.apache.maven.plugins.annotations.Mojo
import org.apache.maven.plugins.annotations.Parameter
import org.apache.maven.project.MavenProject
import org.twdata.maven.mojoexecutor.MojoExecutor
import br.nom.nicola.joao.jnlp.Applicationu45desc
import br.nom.nicola.joao.jnlp.Jar
import br.nom.nicola.joao.jnlp.Jnlp
import br.nom.nicola.joao.jnlp.JnlpJnlpFormat
import br.nom.nicola.joao.jnlp.Property
import java.io.OutputStream
import java.io.ByteArrayOutputStream
import br.nom.nicola.joao.jnlpdependencies.jar.SignatureRemovalDecider
import br.nom.nicola.joao.jnlpdependencies.jar.InterceptingJarOutputStream


@Mojo(name = "build-onejar")
class JnlpDependenciesMavenPlugin extends AbstractMojo {

  @Parameter(property="jnlpDependencies.url",required = true)
  private var jnlpUrl: URL = _

  @Component
  private var session: MavenSession = _

  @Component
  private var project: MavenProject = _
    
  @Component
  private var pluginManager : BuildPluginManager = _;
 
  @Parameter(defaultValue = "${project.basedir}", readonly = true)
  private var basedir: File = _

  @Parameter(defaultValue = "${project.build.directory}", readonly = true)
  private var target: File = _
  
  def info(s : String) = getLog().info("build-onejar: " + s)
  
  def execute(): Unit = {
      import scala.collection.JavaConversions._
      if(jnlpUrl == null) {
        throw new RuntimeException("jnrl url must be given!")
      }
      
      val jnlpXml = XML.load(jnlpUrl)
      val jnlp = scalaxb.fromXML[Jnlp](jnlpXml)
      process(jnlp);//,project)      
      project.getProjectReferences
      
  }
  
  val packSuffix = ".pack.gz"
  
  def process(jnlp : Jnlp/*, project: MavenProject */) : Unit = {
    val mainClass = jnlp.jnlpoption.value match {
      case d : Applicationu45desc =>
         d.mainu45class.getOrElse("")
      case _ => ""
    }
    val tmpDir = new File(target,"jnlp-dependencies")
    tmpDir.mkdirs()
    val codeBase = jnlp.codebase.map(u => new URL(u + "/").toURI.normalize.toURL)
    info(s"codeBase = ${codeBase}")    
    val jars = jnlp.resources.filter(_.os == None).map(_.resourcesoption) flatMap { res =>
      val props = res.map(_.value).collect({ case p : Property => (p.name,p.value) }).toMap      
      res.map(_.value) collect { case x : Jar => (props,x) } 
    }
    val jarMap = jars.map({ case (props,jar) =>
      val pack = props.get("jnlp.packEnabled").map(_.toBoolean).getOrElse(false)
      val suffix = if (pack) { packSuffix } else { "" }
      info(s"jar: download=${jar.download}, href=${jar.href}, main=${jar.main}, part=${jar.part}, size=${jar.size}, version=${jar.version}, pack=${pack}")
      val url = codeBase.map(new URL(_,jar.href + suffix)).getOrElse(new URL(jar.href + suffix))
      (url -> (props,jar))
    }).toMap
                    
    val jarMap2 = jarMap.map({case (url,(props,jar)) =>
      import java.io._
      val name = url.getFile.replaceAll(".*/","")
      val packed = name.endsWith(packSuffix)
      val fname = if (packed) { name.substring(0,name.length() - packSuffix.length) } else { name }
      val f = new File(tmpDir,fname)  
      if(!f.isFile()) {
        val ins = url.openStream()
        info(s"downloading ${url} to ${f}")          
        val bos = new ByteArrayOutputStream()
        val os = if(packed) {
          val js = new InterceptingJarOutputStream(SignatureRemovalDecider,new BufferedOutputStream(new FileOutputStream(f)))
          Pack200.newUnpacker().unpack(new GZIPInputStream(ins), js)
          js.close()
        } else {
          FileUtils.copyInputStreamToFile(ins, f)
        }
      }
      (url -> (f,props,jar))
    })
        
              
    import org.twdata.maven.mojoexecutor.MojoExecutor._
    
    val pl = plugin(
          groupId("br.nom.nicola.joao.fork.org.dstovall"),
          artifactId("onejar-maven-plugin"),
          version("1.4.6-SNAPSHOT")
      ) 
    val config = {
      configuration(
           element(name("mainClass"),mainClass),
           element(name("attachToBuild"),"true"),
				   element(name("filename"),"${project.build.finalName}-onejar.${project.packaging}"),
				   element(name("libs"),
				       element(name("fileSet"),
				           element(name("directory"),"${project.build.directory}/jnlp-dependencies"),
				           element(name("includes"),
				               element(name("include"),"*.jar")
				           )
				       )
				   )				
      )
    }
    executeMojo(
      pl,
      goal("one-jar"),
      config,
      executionEnvironment(
          project,
          session,
          pluginManager
      )
    )     
    
  }

}



