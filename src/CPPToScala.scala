package converter


// import collection.JavaConversions._
import scala.collection.JavaConverters._
import java.nio.file.Path
import java.nio.file.Files
//BufferedWriter
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
//import java.io.File
import java.io.File


object CPPToScala {

  // Utility //

  def toFile(nm: String) : File = {
    val f = new File(nm)
    if (!f.exists()) Trace.terminate("file doesn't exist")
    val p: Path = f.toPath
    val filename = p.getFileName().toString
    val (name, ext) = filename.splitAt(filename.lastIndexOf('.'))
    if (!(
      ext == ".h"
	|| ext == ".cpp"
    )) Trace.terminate(s"file extension does't match '.h' or  '.cpp': $ext")

    f
  }

  def toOutputFile(f: File, extension: String) : File = {
    val p: Path = f.toPath
    val filename = p.getFileName().toString
    val (name, ext) = filename.splitAt(filename.lastIndexOf('.'))
    val prnt = p.getParent()
    val fOut = prnt.resolve(name + '.' + extension).toFile()

    //if(fOut.exists) Trace.terminate(s"proposed output file exists!: $fOut")
    fOut
  }

  def readFile(f: File, charset: Charset) : String = {
    val lines = Files.readAllLines(f.toPath, charset).asScala
    val b = new StringBuilder()
    lines.foreach{ l => b ++= l; b ++= System.lineSeparator() }
    
    b.result
  }

  def writeFile(f: File, txt: String, charset: Charset) {
    val  writer = Files.newBufferedWriter(f.toPath, charset)

    try {
      writer.write(txt, 0, txt.length())
    }
    catch {
      case e: Exception => Trace.critical("Problems with file writing", e)
    }
    finally {
      writer.close()
    }
  }


  def convert(opts: Map[String, Boolean], fileStr: String) {

    val parser = new CPPParser()

    val charset = StandardCharsets.UTF_8

    val fIn = toFile(fileStr)
    val fOut = toOutputFile(fIn, "scala")
    val dataIn = readFile(fIn, charset)

    //val ast =
    val ast =
      if (opts("--parserTrace")) parser.traceCPP(dataIn)
      else parser.parseCPP(dataIn)

    //println("ast:")
    //println(ast)

    if (AST.indentWierd() != 0) {
      val msg =
        if (AST.indentWierd() > 0) "missing close brackets"
        else "missing open brackets"
      Trace.warning(s"parsing caused uneven indents: $msg")
    }

    Trace.info(s"unparsed lines: ${parser.unparsedLines}")

    val structurePhase = new phase.Structure(
      opts("--makeObjects"),
      !opts("--noNamespacing")
    )
    val finalAST = structurePhase.run(ast)

    val ns = structurePhase.namespaces()
    val nsOut =
      if (ns.isEmpty) "none"
      else ns.mkString(", ")

    Trace.info(s"namespaces parsed: $nsOut")

    val b = new StringBuilder()
    val bRet = AST.toScala(
      b,
      finalAST,
      opts("--commentUnparsed"),
      if(opts("--tabBy2")) 2 else 4,
      opts("--verticalParams")
    )

    writeFile(fOut, bRet.result, charset)
    Trace.plain(s"output written to $fOut")
  }

  def run(opts: Map[String, Boolean], fileStrs: Seq[String]) {
    Trace.info(s"starting...")
    fileStrs.foreach(convert(opts, _))
  }

  def main(args: Array[String]) {

    val cl = new common.CommandLineOptionParser(
      codeName = "cppToScalaConverter",
      summary = "attempt Scala code from C++ code",
      usage = "CPPToScala",
      notes = "Produces a file of information from the given file.",
      version = Version.version,
      credits = "Copyright rcrowther 2015",
      callback = run _
    )


    cl += (
      long = "makeObjects",
      short = "co",
      description = "generate code for (the shell of) complementary objects"
    )

    cl += (
      long = "commentUnparsed",
      short = "c",
      description = "comment unparsed lines"
    )

    cl += (
      long = "verticalParams",
      short = "vp",
      description = "arrange method parameters vertically (default = horizontal)"
    )

    cl += (
      long = "tabBy2",
      short = "t-",
      description = "tab output indents by 2 spaces (default = 4)"
    )

    cl += (
      long = "noNamespacing",
      short = "nns",
      description = "do not place namespace indications on namespaced code"
    )

    cl += (
      long = "parserTrace",
      short = "t",
      description = "generate a trace from the parser (mainly for internal debugging)"
    )

    cl.parse(args)

  }

}


