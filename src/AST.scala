package converter


/**
  * These case classes form the nodes of the AST.
  */
abstract class AstNode

case class RootNode(elems: List[AstNode]) extends AstNode

/** Carries a mark of some kind.
  *
  * An alternative to `IdentifierNode` for Pointer marks, privacy
  * marks, etc. Only used when a place in a larger node will explain
  * it's meaning.
  */
// *NOT* a case class, so instances will compare
class MarkNode() extends AstNode
case class KeywordNode(word: String) extends AstNode

case class NumericNode(value: BigDecimal) extends AstNode

case class IdentifierNode(text: String) extends AstNode
case class IdentifierListNode(elems: List[IdentifierNode]) extends AstNode


// Reused in methods fields and classes.
case class ModifierNode(name: String) extends AstNode
case class ModifierListNode(elems: List[ModifierNode]) extends AstNode

/**
  * @param ns the namespace. This is an Option in an
  *  Option - if only the first option was available,
  *  a global namespace was parsed ::...<identifier>
  * @param modifiers modify the type itself, eg, 
  * 'long', 'unsigned'
  * @param name of the type, could be KeywordNode, if
  *  a built-in type, or IdentifierNode
  */
case class TypeIdNode(
  ns: Option[Option[IdentifierNode]],
  modifiers: List[KeywordNode],
  name: AstNode,
  refMark: Option[MarkNode],
  genericParams: Option[List[IdentifierNode]]
)


/** Data for a type.
  *
  * The id field is a `TypeIdNode`.
  * @param modifier modify handling of the type, i.e, 'const', 'volatile'
  * @param cvMods modify  of the type, eg, 'const', 'virtual'
  */
case class TypeMarkNode(
  modifier: Option[KeywordNode],
  id: TypeIdNode
)

/** Joins a type to an identifier.
  *
  * Used currently for parameters, but may have other uses.
  */
case class TypeIdPairNode(typ: TypeMarkNode, name: IdentifierNode)
//case class ParameterListNode(elems: List[ParameterNode]) extends AstNode

case class PrivacyMarkNode(name: String) extends AstNode


/** A namespace declaration.
  *
  * @param ns the namespace name.
  * @param params temp/ated (generic) parms
  */
case class NamespaceMarkNode(
  ns: IdentifierNode,
  params: Option[List[IdentifierNode]]
) extends AstNode

/** A namespace definition.
  */
case class NamespaceSayNode(
  name: Option[IdentifierNode],
  body: RootNode
) extends AstNode

/** A `for` loop definition.
  */
case class ForSayNode(
  init: AssignmentSayNode,
  test: UnparsedLineNode,
  repeatCode : UnparsedLineNode,
  body: RootNode
)  extends AstNode

// Used where a fieldSay has no expression (so is a FieldKnow)
case class NoExpressionNode() extends AstNode

case class ExpressionSayNode(expr: AstNode) extends AstNode

case class AssignmentSayNode(
  assignee: AstNode,
  expr: ExpressionSayNode
) extends AstNode

/** Field identifier.
  *
  * e.g. private const bool<T> a
  */
case class FieldKnowNode(
  privacy: MarkNode,
  compileMod: Option[KeywordNode],
  storageMod: Option[KeywordNode],
  returnType: TypeMarkNode,
  namespace: Option[NamespaceMarkNode],
  name: IdentifierNode
) extends AstNode
{
  def nsAsText : String = {
    if( namespace != None ) namespace.get.ns.text
    else ""
  }
}

/** Field definition.
  *
  * e.g. x = ...;
  *
  * @param tail can be a NoExpressionNode, or UnparsedLineNode 
  */
case class FieldSayNode(
  id: FieldKnowNode,
  tail : AstNode
) extends AstNode
{
  def nsAsText : String = {
    val namespace = id.namespace
    if( namespace != None ) namespace.get.ns.text
    else ""
  }
}



/** A class definition.
  *  class a() {}
  */
case class ClassSayNode(
  privacy: MarkNode,
  inherit: Seq[String],
  modifiers: Seq[String],
  genericParams: Option[List[IdentifierNode]],
  name: IdentifierNode,
  body: RootNode
) extends AstNode

/** A trait definition.
  *
  * For Scala.
  *
  *  trait a {}
  */
case class TraitSayNode(
  privacy: MarkNode,
  inherit: Seq[String],
  modifiers: Seq[String],
  name: IdentifierNode,
  body: RootNode
) extends AstNode

/** An object definition.
  *
  * For Scala.
  */
case class ObjectSayNode(
  genericParams: Option[List[IdentifierNode]],
  name: IdentifierNode,
  body: RootNode
) extends AstNode


/** A method definition.
  *  e.g. bool a() {}
  *
  * @param body any astnode. In practice, only root node and abstractSay (notated '=0') are present. 
  */
case class MethodSayNode(
  privacy: MarkNode,
  compileMod: Option[KeywordNode],
  storageMod: Option[KeywordNode],
  returnType: TypeMarkNode,
  namespace: Option[NamespaceMarkNode],
  name: IdentifierNode,
  params: List[TypeIdPairNode],
  body: AstNode
) extends AstNode
{
  def nsAsText : String = {
    if( namespace != None ) namespace.get.ns.text
    else ""
  }
}


/** A method declaration.
  *
  * e.g. a()
  *
  * Method calls parse like constructors. See the separate node `ConstructorTryNode`
  */
case class ConstructorTryNode(
  namespace: Option[NamespaceMarkNode],
  name: IdentifierNode,
  params: List[TypeIdPairNode],
  initializers: Option[List[(IdentifierNode, IdentifierNode)]],
  body: Option[RootNode]
) extends AstNode


// -------------
// Preprocessor
//--------------
case class PreprocessLineNode(name: Option[KeywordNode], line: UnparsedLineNode) extends AstNode
case class PreprocessKeyword(name: String) extends AstNode

// -----------
// Text nodes
//------------
case class BlockCommentNode(text: String) extends AstNode
case class LineCommentNode(text: String) extends AstNode

/** An uparsed line which is commented.
  */
case class UnparsedLineNode(text: String) extends AstNode

/** Code in undetermined brackets
  *
  * Used for a measure of error recovery.
  */
case class FreeBracketedNode(child: RootNode) extends AstNode

/** Contents of abstract classes, methods, fields.
  */
case class AbstractSayNode() extends AstNode



object Node {


  // Enumerated types

  private[this] val accessThing = new MarkNode()
  def Access: MarkNode = accessThing

  private[this] val addressThing = new MarkNode()
  def Address: MarkNode = addressThing

  private[this] val publicThing = new MarkNode()
  def Public: MarkNode = publicThing

  private[this] val protectedThing = new MarkNode()
  def Protected: MarkNode = protectedThing

  private[this] val privateThing = new MarkNode()
  def Private: MarkNode = privateThing


  // Other  presets //


  /** Represent abstract statements.
    *
    * Used for both methods and classes. 
    */
  private[this] val abstractSayNodeThing = AbstractSayNode()
  def abstractSay : AbstractSayNode = abstractSayNodeThing


  private[this] val noExpressionNodeThing = NoExpressionNode()
  def noExpression : NoExpressionNode = noExpressionNodeThing



  // Alternative constructors //

  def complexTypeId(
    ns: Option[Option[IdentifierNode]],
    name: IdentifierNode,
    refMark: Option[MarkNode],
    genericParams: Option[List[IdentifierNode]]
  ) = new TypeIdNode(ns, List.empty[KeywordNode], name, refMark, genericParams)



  def simpleTypeId(
    modifiers: List[KeywordNode],
    name: KeywordNode,
    refMark: Option[MarkNode]
  ) = new TypeIdNode(None, modifiers, name, refMark, None)



  def classSay(
    genericParams: Option[List[IdentifierNode]],
    name: IdentifierNode,
    body: RootNode
  ) = new ClassSayNode(
    privacy = Node.Public,
    inherit = Seq.empty[String],
    modifiers = Seq.empty[String],
    genericParams,
    name,
    body
  )

  def fieldKnow(
    compileMod: Option[KeywordNode],
    storageMod: Option[KeywordNode],
    returnType: TypeMarkNode,
    namespace: Option[NamespaceMarkNode],
    name: IdentifierNode
  ) = new FieldKnowNode(
    privacy = Node.Public,
    compileMod,
    storageMod,
    returnType,
    namespace,
    name
  )


  def assignmentSay(
    assignee: AstNode,
    expr: ExpressionSayNode
  ) = new AssignmentSayNode(
    assignee,
    expr
  )

  def methodSay(
    compileMod: Option[KeywordNode],
    storageMod: Option[KeywordNode],
    returnType: TypeMarkNode,
    namespace: Option[NamespaceMarkNode],
    name: IdentifierNode,
    params: List[TypeIdPairNode],
    body: AstNode
  ) = new MethodSayNode(
    privacy = Node.Public,
    compileMod,
    storageMod,
    returnType,
    namespace,
    name,
    params,
    body
  )

}//Node



object AST {



  //------------------
  // Indents and lines
  //-------------------

  var indent = 0
  var indentTabSize = 2
  var indentDecWarning = false

  def indentWierd() : Int = {
    if (indent > 0) 1
    else {
      if (indentDecWarning) -1
      else 0
    }
  }

  def indentInc {
    indent += indentTabSize
  }

  def indentDec {
    if (indent > 0) indent -= indentTabSize
    else indentDecWarning == true
  }

  def indent(b: StringBuilder) {
    b ++= {" " * indent}
  }
  def indent(b: StringBuilder, text: String) {
    b ++= {" " * indent}
    b ++= text
  }
  def newIndent(b: StringBuilder, text: String) {
    b ++= "\n"
    b ++= {" " * indent}
    b ++= text
  }

  def newIndent(b: StringBuilder) {
    b ++= "\n"
    b ++= {" " * indent}
  }

  def reindentTextLines(str: String) : String = {
    val b = new StringBuilder()
    var lineStart = false

    // trim ends,
    // then we're certain of the format, whatever the source.
    str.trim.foreach { c =>

      if (!lineStart && c != '\r' && c != '\n') b += c
      else {
        if (c == '\r' | c == '\n') {
          lineStart = true
          b += c
          b ++= {" " * (indent + 2)}
        }
        else {
          if (c != ' ') {
            lineStart = false
            b += c
          }
        }
      }

    }

    b.result
  }

  def commentedLine(b: StringBuilder, txt: String) = {
    indent(b)
    b ++= "//"
    b ++= txt
    b += '\n'
  }

  def printLine(b: StringBuilder, txt: String) = {
    indent(b)
    b ++= txt
    b ++= "\n"
  }


  def camelCase(text: String)
      : String =
  {
    val b = new StringBuilder()
    b.sizeHint(text.size)
    var cOut = ' '
    var capitalizeNext = false
    text.foreach { c =>
      if (c != '_' && c != ' ') {
        cOut =
          if(capitalizeNext) {
            capitalizeNext = false
            c.toUpper
          }
          else c
        b += cOut
      } else capitalizeNext = true

    }

    b.result
  }


  //-------------------
  // Specific handlers
  //-------------------

  def privacyWrite(b: StringBuilder, privacy: MarkNode) {
    // dont print "public" - is default in Scala
    if(privacy == Node.Protected) b ++= "protected "
    if(privacy == Node.Private) b ++= "private "
  }

  /** Block of code surrounded by brackets.
    *
    * Adds intents to the brackets and contents, finishes with a newline.
    */
  def codeBlock(b: StringBuilder, n: List[AstNode]) {
    indent(b)
    b ++= "{\n"
    indentInc
    n.foreach(toScalaRaw(b, _))
    indentDec
    indent(b, "}\n")
  }

  /** Block of code.
    *
    * Written at current mark, no brackets. See `codeBlock`.
    */
  def simpleCodeBlock(b: StringBuilder, n: List[AstNode]) {
    n.foreach(toScalaRaw(b, _))
  }

  def methodModifierWrite(
    b: StringBuilder,
    compileMod: Option[KeywordNode],
    storageMod: Option[KeywordNode]
  )
  {
    // That compileMod may be "virtual" is interesting,
    // but used elsewhere
    // That storageMod may be "static" or "extern"
    // is interesting, but used elsewhere
    if (compileMod != None && compileMod.get.word == "inline") {
      indent(b)
      b ++= "@inline\n"
    }

  }

  /** Writes builtin typess.
    *
    * @param preserve comment, but do not drop, unknown types
    * @return true if a type was printed (commented or not), else false.
    */
  private def builtintypeAscriptionWrite(
    b: StringBuilder,
    mods: List[KeywordNode],
    n: KeywordNode,
    preserve: Boolean,
    withIndent: Boolean
  )
      : Boolean =
  {

    // Nasty little helper, but worse to write it out.
    def write(str: String) : Boolean = {
      if(withIndent) indent(b)
      b ++= str
      true
    }
    // The argument against modifiers goes like this;
    // - Unsigned is usually used to both roughly double capacity and limit
    // numeric range. However, the only C++ guarentee is that it is at least the
    // same size as signed.
    // (short) int - At least 16 bits (in Scala, that's Short)
    // (long) int - At least 32 bits (in Scala, that's Int)
    // long long int  At least 64 bits  (in Scala, that's Long)

    // Actions:
    // - signed/unsigned is ignored.
    // - more than two mentions of long makes it Scala Long
    // else long is Scala Int
    // - defined chars are commented, for now.

    var longCount = mods.count( "long" == _)

    val ret =
      n.word match {
        //case "void" =>
        case "bool" => write(": Boolean")
        //case "char" =>
        //case "char16_t" =>
        //case "char32_t" =>
        case "short" => write(": Short")
        case "int" => write(": Int")
        case "long" => {
          if (longCount > 1)  write(": Long")
          else write(": Int")
        }
        case "float" => write(": Float")
        case "double" => write(": Double")
        case "auto" => false
        case _ => {
          if(!preserve) false
          else {
            write(": ?" + n.word)
          }
        }
      }

    ret
  }


  /** Write a type ascription.
    *  e.g. ": Int"
    * 
    * writes ":? <name>" if the type is not recognised.
    *
    * @return true if a type was printed (commented or not), else false.
    */
  def typeAscriptionWrite(
    b: StringBuilder,
    n: TypeMarkNode,
    preserve: Boolean,
    withIndent: Boolean
  )
      : Boolean =
  {

    // The modifiers here are const and volatile, and Scala
    // has no immediate use for either (const perhaps).
    val idNode = n.id

    // ns will be used elsewhere
    // do nothing with refMark (& and *)?
    val ret =
      idNode.name match {
        case n: KeywordNode => {
          // Ok, it was a builtin type like bool
          builtintypeAscriptionWrite(b, idNode.modifiers, n, preserve, withIndent)
        }
        case IdentifierNode(text) => {

          // Ok, it was a code-defined type
          if(withIndent) indent(b)
          b ++= ": "
          b ++= text
          if (idNode.genericParams != None) {
            genericList(b, idNode.genericParams.get)
          }
          true
        }
      }
    ret

  }


  /** Write a typed identifier.
    *
    * Currently means parameters.
    *
    *  e.g. "house: Int"
    */
  private def typedIdentifierWrite(b: StringBuilder, n: TypeIdPairNode) {
    b ++= n.name.text
    typeAscriptionWrite(b, n.typ, true, false)
  }



  /** Write a list of parameters.
    *
    * NB: Internally, parameters are called `TypeIdPair`
    *
    * @return true if a list was printed (commented or not), else false.
    */
  def typedIdentifierListWrite(b: StringBuilder, list: List[TypeIdPairNode])
      : Boolean =
  {
    if (list.isEmpty) false
    else {
      var first = true

      b += '('

      // Temporary
      if (verticalParams) {
        indentInc
      }

      list.foreach{ elem =>
        if (first) first = false
        else b ++= ", "
        if (verticalParams) {
          newIndent(b)
        }
        typedIdentifierWrite(b, elem)
      }

      // Reset increment
      if (verticalParams) {
        indentDec
        newIndent(b)
        b ++= ")"
        b ++= "\n"
      }
      else   b ++= ")"

      true
    }
  }

  /** Write an inheritance list.
    *
    * NB: Always vertical
    *
    *  e.g. " extends Slacker with Looser"
    */
  def inheritanceWrite(b: StringBuilder, inherit: Seq[String]) {
    var first = true
    inherit.foreach{ i =>
      if (first) {
        b ++= "extends "
        first = false
      }
      else b ++= "with "
      b ++= i
    }
  }


  /** Write an identifier list as comma-separated items.
    *  e.g. " extends Slacker with Looser"
    */
  def identifierListCommasWrite(b: StringBuilder, list: List[IdentifierNode]) {
    var first = true
    list.foreach{ elem =>
      if (first) first = false
      else b ++= ", "
      b ++= elem.text
    }
  }

  def genericList(b: StringBuilder, list: List[IdentifierNode]) {
    b += '['
    identifierListCommasWrite(b, list)
    b += ']'
  }

  def exprWrite(b: StringBuilder, n: ExpressionSayNode) {
    n.expr match {
      case NumericNode(value) => b append value
      case _ => b ++= "AST Print Warning: Unknown expression node"
    }
  }

  /** Writes a variable.
    *
    * This is the stub part of field declarations e.g. "const bool var move_house"
    */
  //TODO: COuld handle a guess at 'vals' too ('const' info is available...)
  def fieldKnowWrite(b: StringBuilder, n: FieldKnowNode) {

    indent(b)
    privacyWrite(b, n.privacy)

    methodModifierWrite(b, n.compileMod, n.storageMod)

    b ++= "var "
    b ++= n.name.text
    typeAscriptionWrite(b, n.returnType, false, false)
    //b ++= n.tail.text
  }

  /** Process preprocessor 'import' lines.
    *
    * Removes quotes and replaces slashes with periods.
    */
  def importTextProcess(text: String)
      : String =
  {
    val sstr = text.trim
    val limit = sstr.length() - 1
    val str =
      if(sstr.charAt(0) == '"' && sstr.charAt(limit) == '"') sstr.substring(1, limit)
      else sstr
    str.replace("/", ".")
  }



  //------------------
  // Recursive writer
  //------------------

  var commentUnparsedLines = false
  var verticalParams = false

  def toScala(
    b: StringBuilder,
    node: AstNode
  )
      : StringBuilder =
  {
    indent = 0
    toScalaRaw(b, node)
  }

  def toScala(
    b: StringBuilder,
    node: AstNode,
    commentUnparsedLines : Boolean,
    indentTabSize: Int,
    verticalParams : Boolean
  )
      : StringBuilder =
  {
    indent = 0
    this.commentUnparsedLines = commentUnparsedLines
    this.indentTabSize = indentTabSize
    this.verticalParams = verticalParams
    toScalaRaw(b, node)
  }

  private  def toScalaRaw(
    b: StringBuilder,
    node: AstNode
  ) : StringBuilder = {
    

    node match {

      case RootNode(elems) => elems.foreach(toScalaRaw(b, _))

      case PreprocessLineNode(name, UnparsedLineNode(text)) => {
        if (name != None) {
          val keyword = name.get.word
          keyword match {
            case "include" => {
              b ++= "import "
              b ++= importTextProcess(text)
              b ++= "\n"
            }
            // Unmatched preprocessing keyword. Print name and text.
            case _ => commentedLine(b, "#? " + keyword + text)
          }
        }
        else {
          commentedLine(b, "#? " + text)
        }
      }

      // Should be treated by phases
      // Here for testing
      case PrivacyMarkNode(text) => {
        //newIndent(b, "//")
        //b ++= text
      }

      case NamespaceSayNode(id, RootNode(elems)) => {
        indent(b)
        // Place in output for reference
        // ...but ignore if there's no name attacted.
        if (id != None && !id.get.text.isEmpty) {
          b ++= "// namespace "
          b ++= id.get.text
          b += ' '
          codeBlock(b, elems)
        }
        else simpleCodeBlock(b, elems)

        // Extra newline
        b += '\n'
      }

      case ClassSayNode(privacy, inherit, modifiers, genericParams, name, body) => {
        b ++= "class "
        b ++= name.text


        if (genericParams != None) {
          genericList(b, genericParams.get)
        }

        inheritanceWrite(b, inherit)

        b += '\n'
        codeBlock(b, body.elems)
      }

      case ConstructorTryNode(namespace, name, params, initializers, body) => {
        //   namespace: Option[NamespaceMarkNode],
        //name: IdentifierNode,
        //params: List[TypeIdPairNode],
        //initializers: Option[List[(IdentifierNode, IdentifierNode)]],
        //body: Option[RootNode]

        indent(b)
        b ++= "def this"

        // Write params
        typedIdentifierListWrite(b, params)


        b ++= " = ?this()\n"
        if(body != None) {
          codeBlock(b, body.get.elems)
          b ++= "\n"
        }
        b ++= "\n"

      }

      case MethodSayNode(privacy, compileMod, storageMod, returnType, namespace, name, params, body) => {
        newIndent(b)
        privacyWrite(b, privacy)

        methodModifierWrite(b, compileMod, storageMod)

        b ++= "def "
        b ++= name.text

        if(namespace != None) {
          val data = namespace.get
          if (data.params != None) {
            genericList(b, data.params.get)
          }
        }

        // Write params
        typedIdentifierListWrite(b, params)



        // If a type return was written
        // and if not known abstract...
        // Indent ia little if vertical
        if (verticalParams) indentInc
        if (
          typeAscriptionWrite(b, returnType, false, verticalParams)
            && body != Node.abstractSay
        ) {
          b ++= " ="
          if (verticalParams) {
            b ++= "\n"
          }
        }
        if (verticalParams) indentDec

        // Without vertical, start block on new line
        if (!verticalParams) {
          b ++= "\n"
        }


        body match {
          case RootNode(elems) => codeBlock(b, elems)
          case AbstractSayNode() => {
            // One of Scala's lovely features - do next to nothing.
            b ++= "\n"
          }
        }

        // Extra newline
        b ++= "\n"
      }

      case FieldSayNode(id, tail) =>
        //(privacy, compileMod, storageMod, returnType, namespace, name) =>
        {
          fieldKnowWrite(b, id)

          // if the field-know stood alone, then it has no significant tail
          tail match {
            case UnparsedLineNode(text) =>
              b ++= " = "
              b ++= text
            case _ =>
          }

          // Not an extra line, fieldKnowWrite does not progress.
          b ++= "\n"
        }

      case LineCommentNode(text) => commentedLine(b, text)

      case BlockCommentNode(text) => {
        indent(b, "/*")
        b ++= reindentTextLines(text)
        // Drop off the block, add the missing tail and a newline.
        b += '\n'
        b ++= {" " * (indent + 2)}
        b ++= "*/\n"
      }

      case node : FreeBracketedNode => {
        // Outer data is not part of the body
        // and always exists with the node.
        codeBlock(b, node.child.elems)
      }

      // Unrecognised but significant line
      // Likely a skipped line in a code block
      case UnparsedLineNode(text) => {
        if(commentUnparsedLines) commentedLine(b, text)
        else printLine(b, text)
      }



        // Scala only //

      case ObjectSayNode(genericParams, name, body) => {
        b ++= "object "
        b ++= name.text
        if (genericParams != None) {
          genericList(b, genericParams.get)
        }
        b += '\n'
        codeBlock(b, body.elems)
      }



      // //
      case ForSayNode(init, test, repeatCode, body) => {

        // Init value
        val initField = init.assignee.asInstanceOf[FieldKnowNode]

        fieldKnowWrite(b, initField)
        b ++= " = "
        exprWrite(b, init.expr)

        // Write the loop
        newIndent(b, "while(")
        b ++= test.text
        b ++= ")\n"

        // Can't codeBlock cause we need an insert?
        //val enhancedBlock = RootNode()
        codeBlock(b, body.elems :+ repeatCode)
      }


      case _ => // Error? Privacy not though.


    }// match

    b
  }


}//AST

