package converter
package phase

import converter.AST


final class Structure(
  val makeComplementaryObjects: Boolean,
  val generateNamespaces : Boolean
)
{

  private var currentPrivacy: MarkNode = Node.Public
  private var namespaceListBuilder = collection.mutable.Set.newBuilder[String]

  def namespaces() : collection.mutable.Set[String] = namespaceListBuilder.result()

  private def groupNamespaces(elems: List[AstNode])
      : List[AstNode] =
  {

    if (elems.isEmpty) elems
    else {

      // Annoyingly tricky data
      // - the field or method before any change of field or method namespace
      var prevNamespace = ""
      var first = true

      val grpB = List.newBuilder[AstNode]
      val prev = Node.abstractSay
      val namespaceB = List.newBuilder[AstNode]
      val intermediaryB = List.newBuilder[AstNode]

      def loadGroup(namespace: String, l: List[AstNode]) {
        if(!namespace.isEmpty) {
          grpB += NamespaceSayNode(
            name = Some(IdentifierNode(namespace)),
            body = RootNode(l)
          )
        }
        else grpB ++= l
      }

      elems.foreach{ node =>

        node match {
          case n: MethodSayNode => {
            if (!first) {
              if(n.nsAsText != prevNamespace) {
                loadGroup(prevNamespace, namespaceB.result)
                loadGroup("", intermediaryB.result)
                intermediaryB.clear
                namespaceB.clear
                prevNamespace = n.nsAsText
              }
              else {
                namespaceB ++= intermediaryB.result
                intermediaryB.clear
              }
            }
            else {
              first = false
              prevNamespace = n.nsAsText
              loadGroup("", intermediaryB.result)
              intermediaryB.clear
            }

            namespaceB += n
          }

          case n: FieldSayNode =>  {
            if (!first) {
              if(n.nsAsText != prevNamespace) {
                loadGroup(prevNamespace, namespaceB.result)
                loadGroup("", intermediaryB.result)
                intermediaryB.clear
                namespaceB.clear
                prevNamespace = n.nsAsText
              }
              else {
                namespaceB ++= intermediaryB.result
                intermediaryB.clear
              }
            }
            else {
              first = false
              prevNamespace = n.nsAsText
              loadGroup("", intermediaryB.result)
              intermediaryB.clear
            }

            namespaceB += n
          }

          case n: AstNode => intermediaryB += n

        }
      }

      // Empty the builders
      loadGroup(prevNamespace, namespaceB.result)
      loadGroup("", intermediaryB.result)

      grpB.result
    }
  }


  private def appendComplementaryObjects(elems: List[AstNode])
      : List[AstNode] =
  {

    val names: Seq[String] = elems.collect{ case ClassSayNode(_, _, template, name, _, _) => name.text }

    val newObjects = names.map{ name =>
      new ObjectSayNode(
        genericParams = None,
        name = new IdentifierNode(name),
        body = new RootNode(List.empty[AstNode])
      )
    }
    
    elems ++ newObjects
  }

  // This extra help for root treatment is needed?
  // ...though messy
  private def root(n: RootNode) : RootNode = {
    // scan all lower elems to trigger phase code
    val phasedElems: List[AstNode] = n.elems.map(run)
    //println(s"RootNode size: ${n.elems.size}")

    val gElems: List[AstNode] =
      if (generateNamespaces) groupNamespaces(phasedElems)
      else phasedElems

    // if necessary, check for classes and add complementary
    // objects.
    // TODO: Add constructors
    val coElems: List[AstNode] =
      if(!makeComplementaryObjects) gElems
      else appendComplementaryObjects(gElems)


    RootNode(coElems)
  }

  /** Returns a tree with structural conversions.
    *
    * Spreads privacy marks acrosss methods and fields.
    * Constructs, if requested, complementtary objects.
    */
  def run(node: AstNode): AstNode = {
    node match {


      case n: RootNode => {
        root(n)
      }

      case n: NamespaceSayNode => {
        // println(s"namespace: ${n.body.elems.size.toString}")
        n.copy(body = root(n.body))
      }

      //case PreprocessLineNode(node: AstNode, UnparsedLineNode(text)) =>
      case n : PrivacyMarkNode => {
        n.name match {
          case "public" => currentPrivacy = Node.Public
          case "protected" => currentPrivacy = Node.Protected
          case "private" => currentPrivacy = Node.Private
        }
        n
      }

      case n: ClassSayNode => {
        // Return the node, treating the body
        n.copy(body = root(n.body))
      }

      case n: MethodSayNode =>  {
        val namespace = n.namespace
        if(namespace != None) {
          namespaceListBuilder += namespace.get.ns.text
        }
        val newBody =
          n.body match {
            case n: RootNode => root(n)
            case _ => n.body
          }
        n.copy(
          privacy = currentPrivacy,
          body = newBody
        )
      }

      case n: FieldSayNode =>  {
        val id = n.id
        val namespace = id.namespace
        if(namespace != None) {
          namespaceListBuilder += namespace.get.ns.text
        }
        n.copy(id = n.id.copy(privacy = currentPrivacy))
      }

      //case LineCommentNode(text) =>
      //case node : BracketedChildNode =>
      case n: AstNode => n
    }
  }

}//Structure

