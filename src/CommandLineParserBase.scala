package common

abstract class OptionEntryBase (
  val parameter: String,
  val description: String
)

/** Common activity for commandline parsers.
  *
  * This mainly handles the construction of help messages.
  */
abstract class CommanLineParserBase[A <: OptionEntryBase](
  protected val codeName: String,
  protected val summary: String,
  protected val usage: String,
  protected val notes: String,
  protected val credits : String
)
{
  protected var options : IndexedSeq[A]

  /** Returns the index of the last space in a relative slice of a string.
    *
    * For example, in an incrementing or recursive loop this method can
    * extract text columns.
    *
    * @param str to find index in
    * @param fromIndex start of slice
    * @param width of slice
    * @return -1 if char not found or fromIndex > str.lemgth. Otherwise
    * returns the index of the last space within the string slice. If
    * the string will finish within the slice ((fromIndex + width) >
    * str.length) then the index of the last char is returned.
    */
  protected def lastIndexOfDelimitedSpace(str: String, fromIndex: Int, width: Int)
      : Int =
  {
    if (fromIndex > str.length) -1
    else {
      var relLimit = fromIndex + width
      if (relLimit > str.length) str.length
      else str.lastIndexOf(' ', relLimit)
    }
  }

  /** Overall width of printing column.
    *
    * Can be overridden, from the preset of 72.
    */
  protected val  blockWidth = 72

  private def blockSplit(
    initIndent: Int,
    colIndent: Int,
    str: String
  )
      : StringBuilder =
  {
    val sb =  new StringBuilder("")
    var backCursor = 0
    var colWidth = blockWidth - colIndent
    var strLen = str.length
    val indentStr = "".padTo(colIndent, ' ')

    // If initIdent < colIndent, pad to the column.
    var frontCursor =
      if (initIndent < colIndent){
        sb.append("".padTo(colIndent - initIndent, ' '))
        lastIndexOfDelimitedSpace(str, 0, colWidth)
      }
      else {
        lastIndexOfDelimitedSpace(str, 0, blockWidth - initIndent)
      }

    sb.append(str.slice(backCursor, frontCursor))
    sb.append("\n")
    backCursor = frontCursor + 1
    frontCursor = lastIndexOfDelimitedSpace(str, frontCursor + 1, colWidth)
    while (frontCursor > -1) {
      sb.append(indentStr)
      sb.append(str.slice(backCursor, frontCursor))
      sb.append("\n")
      backCursor = frontCursor
      frontCursor = lastIndexOfDelimitedSpace(str, frontCursor + 1, colWidth)
    }
    frontCursor += str.length + 1
    val tail = str.slice(backCursor, frontCursor)
    if (tail.length > 0) {
      sb.append(indentStr)
      sb.append(str.slice(backCursor, frontCursor))
      sb.append("\n")
    }
    sb
  }


  private def getOptionHelp
      : StringBuilder =
  {
    val sb =  new StringBuilder("")
    options.foreach{ option =>
      sb.append("  ")
      sb.append(option.parameter)
      sb.append(
        blockSplit(option.parameter.length + 2, 30, option.description)
      )
    }
    sb
  }

  protected def printHelp
  {
    val sb =  new StringBuilder("")
    sb.append("Usage: ")
    sb.append(usage)
    sb.append(" [ACTION]\n")
    sb.append(summary)
    sb.append("\n")
    sb.append(blockSplit(0, 0, notes))
    sb.append("\nOptions:\n")
    sb.append(getOptionHelp)
    sb.append(credits)
    System.err.println(sb.result)
  }

  protected def helpPrompt

}//CommanLineParserBase
