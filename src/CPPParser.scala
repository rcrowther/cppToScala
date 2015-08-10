package converter


import org.parboiled.scala._
import java.lang.String
import org.parboiled.errors.{ErrorUtils, ParsingException}

//import AstNode._

class CPPParser extends Parser {

  var unparsedLines = 0

  var balanceBrackets = 0

  def CPP:  Rule1[RootNode] = rule { Spacing ~ TopLevel  ~ EOI}

  /** Parses content.
    *
    * This rule only fails when it reaches EOI.
    *
    * Various treatments are tried on content at the cursor position.
    * If these fail, two methods are used to make progress.
    *
    * - The current line is scanned for LWING. If this succeeds, then
    * the following contents are treated as if in balanced wing
    * brackets, and a balancing bracket is searched for. See
    * `InScopeFreeMatchBrackets`.
    *
    * - A complete line is passed as unparsed, then the parser has
    * another loop, attempting a parse. See `UnparsedOpenWingOrCLine`.
    *
    * A small final rule ensures that an tail material is also sent as
    * an unparsed node.
    */
  // the root rule
  // All contained rules must Spaceing to finish on something significant.
  def TopLevel:  Rule1[RootNode] = rule { zeroOrMore((
    // No following rules should have spacing
    Preprocessor
      | NamespaceSay
      | PrivacyMark
      | LongComment
      | LineComment
      | ClassSay
      | MethodSay
      | FieldSay
      // Try slack things.
      // If slack movement got us to an opening wing, que this.
      | InScopeFreeMatchBrackets
      // Matches everything to an open wing or line end.
      // Open wing is processed on the next pass, as are line end moves.
      | UnparsedOpenWingOrCLine
      // If we reach here the contents, parsed or unparsed, failed on the above rules,
      // so a EOFile is either here or near. If near, this takes up the slack (it fails when positioned at EOFile).
      | EndOfFile
  ) ~ SpacingWithSemiColon ) ~~> RootNode }

  /** Parses the content of matched wing brackets.
    *
    * This rule only fails when it reaches an unbalanced RWING.
    *
    * Various treatments are tried on content at the cursor position.
    * If these fail, two methods are used to make progress.
    *
    * - The current line is scanned for LWING. If this succeeds, then
    * the following contents are treated as if in balanced wing
    * brackets, and a balancing bracket is searched for. See
    * `InScopeFreeMatchBrackets`.
    *
    * - A complete line is passed as unparsed, then the parser has
    * another loop, attempting a parse. See
    * `UnparsedWingsOrCLineInScope`.
    *
    * A small final rule ensures that an tail material is also sent as an
    * unparsed node.   
    */
  def SayList:  Rule1[RootNode] = rule { zeroOrMore((
    // No following rules should have spacing
    Preprocessor
      | NamespaceSay
      | PrivacyMark
      | LongComment
      | LineComment
      | ClassSay
      // Trying to reduce its potential for caos,
      // only allowed nested.
      //| ConstructorSay
      | MethodSay
      | FieldSay
      // Incode rules
      | ForSay
      // Try slack things.
      // If slack movement got us to an opening wing, que this.
      // If it got us to a closing wing, fall out.
      | InScopeFreeMatchBrackets
      // Make a slack movement
      | UnparsedWingsOrCLineInScope
      // If we reach here the contents, parsed or unparsed, failed on the above rule,
      // so a EOScope is either here or near. If near, this takes up the slack (it fails when positioned on a RWING, otherwise moves to the char before).
      //| EndOfScope
  ) ~ SpacingWithSemiColon ) ~~> RootNode  }


  def Preprocessor = rule { "#" ~ SpacingWithinLine ~ optional(PreprocessorKeyword) ~ UnparsedLine  ~~> PreprocessLineNode }



  //-------------------------------------------------------------------------
  //  A.1.1  Lexical elements
  //  Tokens are: Keyword, Identifier, Constant, StringLiteral, Punctuator.
  //  Tokens are separated by Spacing.
  //-------------------------------------------------------------------------



  def IdNondigit = rule { "a" - "z" | "A" - "Z" | "_" }

  def IdChar = rule { "a"-"z" | "A"-"Z" | "0" - "9" | "_" }


  def BalancedBracketBody = rule { LWING ~ SayList ~ RWING }


  /** Balanced wing brackets containing general content.
    *
    */
  def InScopeFreeMatchBrackets = rule{
    LWING ~ SayList ~ RWING ~~> FreeBracketedNode
  }



  //-------------------------------------------------------------------------
  //  A.1.3  Identifiers
  //  The standard does not explicitly state that identifiers must be
  //  distinct from keywords, but it seems so.
  //-------------------------------------------------------------------------

  //TODO: These ought to lead with IdNonDigit,
  // but that makes capture annoying.
  def Identifier = rule { !IsKeyword ~ oneOrMore(IdChar) ~> IdentifierNode ~ Spacing }

  def IdentifierNoSpacing = rule { !IsKeyword ~ oneOrMore(IdChar) ~> IdentifierNode }

  def IdentifierRaw = rule { !IsKeyword  ~ oneOrMore(IdChar) ~ Spacing }


  def IdentifierCommaList : Rule1[List[IdentifierNode]] = rule { zeroOrMore(Identifier, separator = COMMA) }

  /** A reference into or outof a variable.
    *
    */
  def RefMark: Rule1[MarkNode] = rule { ("*" ~ push(Node.Access) | "&" ~ push(Node.Address)) ~ Spacing }





  //-------------------------------------------------------------------------
  //  A.1.7  Punctuators
  //-------------------------------------------------------------------------

  final def EQU        =  ("=" ~ !"=" ~ Spacing)
  final def COMMA      =  ("," ~ Spacing)
  final def COLON      =  (":" ~ !":" ~ Spacing)
  final def SEMICOLON  =  (";" ~ Spacing)
  final def DOUBLECOLON = ("::" ~ Spacing)
  final def LWING      =  ("{" ~ Spacing)
  final def RWING      =  ("}" ~ Spacing)
  final def LPAR       =  ("(" ~ Spacing)
  final def RPAR       =  (")" ~ Spacing)
  final def LT         =  ("<" ~ !"=" ~ Spacing)
  final def GT         =  (">" ~ !"=" ~ Spacing)
  final def LE         =  ("<=" ~ Spacing)
  final def GE         =  (">=" ~ Spacing)
  final def AND        =  ("&" ~ !"&" ~ Spacing)
  final def STAR       =  ("*" ~ !"=" ~ Spacing)



  //-------------------------------------------------------------------------
  //  A.1.2  Keywords
  //-------------------------------------------------------------------------


  final def ALIGNAS   = ("alignas" ~ !IdChar)
  final def ALIGNOF   = ("alignof" ~ !IdChar)
  final def ASM       = ("asm" ~ !IdChar)
  final def AUTO      = ("auto" ~ !IdChar)
  final def BREAK     = ("break" ~ !IdChar)
  final def BOOL      = ("bool" ~ !IdChar)
  final def CASE      = ("case" ~ !IdChar)
  final def CATCH     = ("catch" ~ !IdChar)
  final def CHAR      = ("char" ~ !IdChar)
  final def CHAR16    = ("char16_t" ~ !IdChar)
  final def CHAR32    = ("char32_t" ~ !IdChar)
  final def CLASS     = ("class" ~ !IdChar)
  final def CONST     = ("const" ~ !IdChar)
  final def CONSTEXPR = ("constexpr" ~ !IdChar)
  final def CONST_CAST        = ("const_cast" ~ !IdChar)
  final def CONTINUE  = ("continue" ~ !IdChar)
  final def DECLTYPE  = ("decltype" ~ !IdChar)
  final def DEFAULT   = ("default" ~ !IdChar)
  final def DELETE    = ("delete" ~ !IdChar)
  final def DO        = ("do" ~ !IdChar)
  final def DOUBLE    = ("double" ~ !IdChar)
  final def DYNAMIC_CAST      = ("dynamic_cast" ~ !IdChar)
  final def ELSE      = ("else" ~ !IdChar)
  final def ENUM      = ("enum" ~ !IdChar)
  final def EXPLICIT  = ("explicit" ~ !IdChar)
  final def EXPORT    = ("export" ~ !IdChar)
  final def EXTERN    = ("extern" ~ !IdChar)
  final def FALSE     = ("false" ~ !IdChar)
  final def FLOAT     = ("float" ~ !IdChar)
  final def FRIEND    = ("friend" ~ !IdChar)
  final def FOR       = ("for" ~ !IdChar)
  final def GOTO      = ("goto" ~ !IdChar)
  final def IF        = ("if" ~ !IdChar)
  final def INLINE    = ("inline" ~ !IdChar)
  final def INT       = ("int" ~ !IdChar)
  final def LONG      = ("long" ~ !IdChar)
  final def MUTABLE   = ("mutable" ~ !IdChar)
  final def NAMESPACE = ("namespace" ~ !IdChar)
  final def NEW       = ("new" ~ !IdChar)
  final def NOEXCEPT  = ("noexcept" ~ !IdChar)
  final def NULLPTR   = ("nullptr" ~ !IdChar)
  final def OPERATOR  = ("operator" ~ !IdChar)
  final def PRIVATE   = ("private" ~ !IdChar)
  final def PROTECTED = ("protected" ~ !IdChar)
  final def PUBLIC    = ("public" ~ !IdChar)
  final def REGISTER  = ("register" ~ !IdChar)
  final def REINTERPRET_CAST  = ("reinterpret_cast" ~ !IdChar)
  final def RESTRICT  = ("restrict" ~ !IdChar)
  final def RETURN    = ("return" ~ !IdChar)
  final def SHORT     = ("short" ~ !IdChar)
  final def SIGNED    = ("signed" ~ !IdChar)
  final def SIZEOF    = ("sizeof" ~ !IdChar)
  final def STATIC    = ("static" ~ !IdChar)
  final def STATIC_ASSERT     = ("static_assert" ~ !IdChar)
  final def STATIC_CAST       = ("static_cast" ~ !IdChar)
  final def STRUCT    = ("struct" ~ !IdChar)
  final def SWITCH    = ("switch" ~ !IdChar)
  final def TEMPLATE  = ("template" ~ !IdChar)
  final def THIS      = ("this" ~ !IdChar)
  final def THREADLOCAL       = ("thread_local" ~ !IdChar)
  final def THROW     = ("throw" ~ !IdChar)
  final def TRUE      = ("true" ~ !IdChar)
  final def TRY       = ("try" ~ !IdChar)
  final def TYPEDEF   = ("typedef" ~ !IdChar)
  final def TYPEID    = ("typeid" ~ !IdChar)
  final def TYPENAME  = ("typename" ~ !IdChar)
  final def UNION     = ("union" ~ !IdChar)
  final def UNSIGNED  = ("unsigned" ~ !IdChar)
  final def USING     = ("using" ~ !IdChar)
  final def VIRTUAL   = ("virtual" ~ !IdChar)
  final def VOID      = ("void" ~ !IdChar)
  final def VOLATILE  = ("volatile" ~ !IdChar)
  final def WCHAR_T   = ("wchar_t" ~ !IdChar)
  final def WHILE     = ("while" ~ !IdChar)
  final def COMPLEX   = ("_Complex" ~ !IdChar)
  final def STDCALL   = ("_stdcall" ~ !IdChar)
  final def DECLSPEC  = ("__declspec" ~ !IdChar)
  final def ATTRIBUTE = ("__attribute__" ~ !IdChar)


  // Preprocessor

  final def INCLUDE  = ("include" ~ !IdChar)
  final def DEFINE   = ("pragma" ~ !IdChar)
  final def UNDEF   = ("pragma" ~ !IdChar)
  final def LINE   = ("pragma" ~ !IdChar)
  final def ERROR   = ("pragma" ~ !IdChar)
  final def PRAGMA   = ("pragma" ~ !IdChar)



  //-------------------------------------------------------------------------
  //  Keyword Lists
  //-------------------------------------------------------------------------

  // Preprocessor keywords do not need to appear here?
  def IsKeyword = rule(SuppressSubnodes) {
    (
        "alignas"
        | "alignof"
        | "asm"
        | "auto"
        | "break"
        | "bool"
        | "case"
        | "char16_t"
        | "char32_t"
        | "char"
        | "class"
        | "constexpr"
        | "const_cast"
        | "const"
        | "continue"
        | "decltype"
        | "default"
        | "delete"
        | "double"
        | "do"
        | "dynamic_cast"
        | "else"
        | "enum"
        | "extern"
        | "explicit"
        | "export"
        | "float"
        | "for"
        | "friend"
        | "goto"
        | "if"
        | "int"
        | "inline"
        | "long"
        | "mutable"
        | "namespace"
        | "new"
        | "noexcept"
        | "nullptr"
        | "private"
        | "protected"
        | "public"
        | "register"
        | "reinterpret_cast"
        | "restrict"
        | "return"
        | "short"
        | "signed"
        | "sizeof"
        | "static_assert"
        | "static_cast"
        | "static"
        | "struct"
        | "switch"
        | "thread_local"
        | "true"
        | "try"
        | "typedef"
        | "typeid"
        | "typename"
        | "union"
        | "unsigned"
        | "using"
        | "virtual"
        | "void"
        | "volatile"
        | "wchar_t"
        | "while"
        | "_Bool"
        | "_Complex"
        | "_Imaginary"
        | "_stdcall"
        | "__declspec"
        | "__attribute__"
    ) ~ !IdChar
  }


  /**
    */
  // 7.1.2 Function specifiers
  def MethodCompileHint = rule {
    (
      EXPLICIT
        | VIRTUAL
        | INLINE) ~> KeywordNode ~ Spacing
  }

  /** Allowed before type says ().
    *
    * Only one allowed.
    */
  // 7.1.1 Storage class specifiers
  def StorageAsk: Rule1[KeywordNode] = rule {
    (
      REGISTER
        | STATIC
        | THREADLOCAL
        | EXTERN
        | MUTABLE
    ) ~> KeywordNode ~ Spacing
  }

  def CVAsk = rule {
    ( CONST | VOLATILE ) ~> KeywordNode ~ Spacing
  }

  /** Allowed before type says ().
    */
  // 7.1.6 Type specifiers and 7.1.6.1 The cv-qualifiers
  def SimpleTypeModifierList: Rule1[List[KeywordNode]] = rule {
    zeroOrMore((
      SIGNED
        | UNSIGNED
        | SHORT
        | LONG
    ) ~> KeywordNode ~ Spacing)
  }

  /** Inbuilt types.
    *
    * Several combinations allowed or legal, we allow a lot of illegal
    * by asking only that they are prefixed with `StorageAsk` and
    * `TypeModifierList` types.
    *
    * Only one allowed (for us).
    */
  // 7.1.6.2 Simple type specifiers
  def BuiltinType: Rule1[KeywordNode] = rule {
    (
      CHAR
        | CHAR16
        | CHAR32
        | BOOL
        | SHORT
        | INT
        | LONG
        | FLOAT
        | DOUBLE
        | VOID
        | AUTO
    ) ~> KeywordNode ~ Spacing
  }



  final def PreprocessorKeyword: Rule1[KeywordNode] = rule{ (
    INCLUDE
      | DEFINE
      | UNDEF
      |LINE
      |ERROR
      |PRAGMA
  ) ~> KeywordNode ~ Spacing}



  //-------------------------------------------------------------------------
  //  Templates
  //-------------------------------------------------------------------------

  /** A template (generic) definition.
    *
    * Can be used before methods (unimplemented) and classes.
    *
    * e.g. template <class T>
    */
  def TemplateSay : Rule1[List[IdentifierNode]] = rule { "template" ~ TemplateParameterList }


  def TemplateParameterList: Rule1[List[IdentifierNode]] = rule { "<" ~ IdentifierCommaList ~ ">" ~ Spacing }


  /** Namespace mark.
    *
    * Can be used on methods, fields, type marks.
    *
    * e.g. boost<A, B>::
    */
  // Wrong - namespace seperate from template parameters?
  def NamespaceMark = rule { IdentifierNoSpacing ~ optional(TemplateParameterList) ~ "::" ~~> NamespaceMarkNode }





  //-------------------------------------------------------------------------
  //  Types
  //-------------------------------------------------------------------------


  def PrivacyMark = rule { (PRIVATE  | PROTECTED | PUBLIC) ~> PrivacyMarkNode ~ Spacing ~ COLON }


  /** Builtin type.
    *
    * e.g. unsigned long long* 
    */
  // This, as less:
  // type-specifier: & trailing-type-specifier
  def SimpleType = rule { SimpleTypeModifierList ~ BuiltinType ~ optional(RefMark) ~~> Node.simpleTypeId _ }


  /** Code-defined type.
    *
    * e.g. std::unique_ptr*<RecordFetcher>
    */
  def ComplexType: Rule1[TypeIdNode] = rule {
    optional(optional(Identifier) ~  "::") ~ Identifier ~ optional(RefMark) ~ optional(TemplateParameterList) ~~> Node.complexTypeId _
  }

  /** Mark of a type.
    *
    * e.g. for method return values, parameters, etc.
    */
  def TypeMark: Rule1[TypeMarkNode] = rule { optional(CVAsk)  ~ (SimpleType | ComplexType) ~~> TypeMarkNode }

  // Not on parameters? optional(StorageAsk) ~

  // intializer-clauses
  def TypedIdPair: Rule1[TypeIdPairNode] = rule { TypeMark ~ Identifier ~~> TypeIdPairNode }

  def TypedIdPairList: Rule1[List[TypeIdPairNode]] = rule { zeroOrMore(TypedIdPair, separator = COMMA) }



  //-------------------------------------------------------------------------
  //  Literals
  //-------------------------------------------------------------------------

  def Sign = rule { optional("+" | "-") }
  def Integer = rule { optional(Sign) ~ oneOrMore("0" - "9") }
  def NumericLiteral = rule { Integer ~> ((matched) => NumericNode(BigDecimal(matched))) }



  //-------------------------------------------------------------------------
  //  Expressions and initializers
  //-------------------------------------------------------------------------

  def ExpressionSay = rule { NumericLiteral ~~> ExpressionSayNode }


  /** Root of field/method says.
    *
    * e.g. (daft) virtual static bool boost<A, B>::newTuple
    */
  // The standard? Whats this?
  def FMSay = rule { optional(MethodCompileHint) ~ optional(StorageAsk) ~ TypeMark ~ optional(NamespaceMark) ~ Identifier }

  /** Stub of a field assignment.
    *
    * Internal use. 
    */
  //TOCONSIDER: FMSay is redundant?
  def FieldKnow = rule { FMSay ~~> Node.fieldKnow _ }

  /** A new field with initializing expression.
    */
  // Very similar to a FieldSay, but this is ruthless, handling numeric only for ForSay.
  def FieldAssignmentSay = rule { FieldKnow ~ EQU ~ ExpressionSay ~~> AssignmentSayNode }



  //-------------------------------------------------------------------------
  //  Say
  //-------------------------------------------------------------------------

  def NamespaceSay = rule { NAMESPACE ~ Spacing ~ optional(Identifier) ~ LWING  ~ SayList ~ RWING ~~> NamespaceSayNode }




  /*
   This without the function:
   decl-specifier:
   storage-class-specifier
   type-specifier
   function-specifier
   */
  // Very similar to a FieldAssignmentSay, is one really, but FieldSay mindlessly gathers assignments, making it more general.
  def FieldSay = rule { FieldKnow ~ (CLineEnd ~ push(Node.noExpression) | EQU ~ UnparsedCLine) ~~> FieldSayNode }


  /**
    * i.e. volatile bool mypoint<A, B>::goFor(const A x, const B y)
    */
  // 8.4.1 attribute-specifier-seq opt decl-specifier-seq opt declarator virt-specifier-seq opt function-body
  // '= 0' C++ for 'abstract'
  // TOCONSIDER: Const function modification is not captured here. Probzably should be.
  // TOCONSIDER: If methods were regarded as starting with a FieldKnow...
  def MethodSay = rule { FMSay ~ LPAR ~ TypedIdPairList ~ RPAR ~ optional(CONST ~ Spacing) ~ (LWING  ~ SayList ~ RWING | EQU ~ "0" ~ push(Node.abstractSay)) ~~> Node.methodSay _ }

def ClassBaseModifiers: Rule1[List[KeywordNode]] = rule { zeroOrMore( (PUBLIC | PROTECTED | PRIVATE | VIRTUAL) ~> KeywordNode  ~ Spacing) }

def ClassBase: Rule1[ClassBaseNode] = rule { ClassBaseModifiers ~ Spacing ~ Identifier ~~> ClassBaseNode }

//def ClassBaseList = rule { oneOrMore(ClassBase, separator = Spacing) }

  def ClassSay = rule { optional(TemplateSay) ~ CLASS ~ Spacing ~ Identifier ~ optional(COLON ~ oneOrMore(ClassBase, separator = Spacing)) ~ LWING ~ SayList ~ RWING  ~~> Node.classSay _ }

/**
*/
//TOCONSIDER: 7.1.1
//Change: In C ++ , the static or extern specifiers can only be applied to names of objects or functions
//Not structs
  def StructSay = rule { optional(TemplateSay) ~ STRUCT ~ Spacing ~ Identifier ~ LWING ~ SayList ~ RWING  ~~> Node.structSay _ }

  // 5.2.2 Function call
  // Postfix-expression followed by initializer-clauses


  def ForSay = rule {
    FOR ~ Spacing ~ LPAR ~ FieldAssignmentSay ~ Spacing ~ SEMICOLON ~ oneOrMore(!(LineEnd | ";" ) ~ ANY) ~> UnparsedLineNode ~ SEMICOLON ~ oneOrMore(!(LineEnd | ")" ) ~ ANY) ~> UnparsedLineNode ~ RPAR ~ LWING ~ SayList ~ RWING ~~> ForSayNode
  }



  //-------------------------------------------------------------------------
  //  Call
  //-------------------------------------------------------------------------

  def ConstructorInitiatorSay = rule {Identifier ~ LPAR ~ Identifier ~ RPAR }
  /*
   //TODO: The way to determine this is to keep track of class name declarations,
   // or see if the namespace matches the identifier. 
   // Easy to confuse with a method call or a method template in a header.
   // Beware!
   // TODO: This should handle a multiple parameter constructor initializer,
   // but I'm struggling right now to even try.
   def ConstructorSay = rule { optional( NamespaceMark ) ~ Identifier ~ LPAR ~ TypedIdPairList  ~ RPAR ~ optional(COLON ~ zeroOrMore( ConstructorInitiatorSay, separator = COMMA ) ) ~ optional( LWING ~ SayList ~ RWING ) ~~> ConstructorTryNode }
   */


  //-------------------------------------------------------------------------
  //  Whitespace
  //-------------------------------------------------------------------------

  /** Line ends
    *
    * Matches several variants of line endings, including classic
    * Windows and Unix.
    */
  def LineEnd = rule{ "\r\n" | "\r" | "\n" }
  def CLineEnd = rule{ "\r\n" | "\r" | "\n" | ";" }
  def CommaOrLineEnd = rule { ";" | "\r\n" | "\r" | "\n" }

  def EmptyModifier = rule { push(ModifierNode("")) }

  /** Line ends.
    *
    * Matches several variants of line endings, including classic
    * Windows and Unix.
    *
    * Not spaced.
    */
  def UnparsedLine = rule{ zeroOrMore(!LineEnd ~ ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode ~ LineEnd }

  /** CLine ends.
    *
    * Matches several variants of line endings, including semi-colons.
    *
    * Not spaced.
    */
  // TODO: Should use this a lot more?
  def UnparsedCLine = rule{ zeroOrMore(!CLineEnd ~ ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode ~ CLineEnd }

  /** Line ends and semi-colons.
    * 
    * Matches anything to a line end or semi-colon.
    * Will succesfully read to EOF (is that Parboiled intervention?)
    */
  def UnparsedOpenWingOrCLine = rule{ oneOrMore(!(LineEnd | ";" | "{") ~ ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode }

  def EndOfFile = rule { oneOrMore(ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode }

  /** matches anything leading to a RWING.
    *
    * Will fail if on an RWING, will othewise move to before an RWING.
    */
  def EndOfScope = rule { oneOrMore(!("}") ~ ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode }

  /** Line ends and semi-colons in scope.
    * 
    * Matches anything to a line end or semi-colon.
    * Fails if it meets a RWING. A 
    */
  def UnparsedWingsOrCLineInScope = rule{ oneOrMore(!(LineEnd | ";" | "}" | "{") ~ ANY) ~ run(unparsedLines += 1) ~> UnparsedLineNode }

  def WhiteSpace = rule { anyOf(" \n\r\t\u000B\u000C") } // 7.4.1.10

  def Spacing =  rule { zeroOrMore(WhiteSpace) }
  def SpacingWithinLine =  rule { zeroOrMore(" ") }
  def SpacingWithSemiColon =  rule { zeroOrMore(WhiteSpace | ";") }

  def LongComment = rule { ("/*" ~ zeroOrMore(!("*/") ~ ANY) ~> BlockCommentNode ~ "*/") }   // 6.4.9
    
  def LineComment = rule{ "//" ~ zeroOrMore(!LineEnd ~ ANY) ~> LineCommentNode }



  //-------------------------------------------------------------------------
  //  Main Method
  //-------------------------------------------------------------------------


  /**
    * The main parsing method. Uses a ReportingParseRunner (which only
    * reports the first error) for simplicity.
    */
  def parseCPP(txt: String): AstNode = {
    val parsingResult = ReportingParseRunner(CPP).run(txt)
    parsingResult.result match {
      case Some(astRoot) => astRoot
      case None => throw new ParsingException("CPP source failed to parse:\n" +
          ErrorUtils.printParseErrors(parsingResult))
    }
  }

  import org.parboiled.scala.parserunners._

  def traceCPP(txt: String): AstNode = {
    /*
     Matched
     && Rules.only(LongComment) 
     && Rules.only(InScopeFreeMatchBrackets)
     && Rules.only(InScopeLineEnd) 
     */
    // Match && Rules.only(InScopeFreeMatchBrackets)
    val parsingResult = TracingParseRunner(CPP).filter(
      Matched &&
        // Rules.only(UnparsedOpenWingOrCLine)
        // Rules.only(InScopeFreeMatchBrackets)
        Rules.only(FieldSay)
    ).run(txt)
    parsingResult.result match {
      case Some(astRoot) => astRoot
      case None => throw new ParsingException("CPP source failed to parse:\n" +
          ErrorUtils.printParseErrors(parsingResult))
    }
  }

}//CPPParser
