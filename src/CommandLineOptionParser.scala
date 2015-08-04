package common

final class OptionEntryTarget  (
  val long: String,
  val short: String,
  description: String,
  var value: Boolean
)
    extends OptionEntryBase(parameter = short + "  " + long, description = description)
{
}


/** Switch argument commandline parser with one parameter.
  *
  * This parser only calls back on one method, and sends all the
  * details to that method as a x(options:Map[String, Boolean],
  * arg:String). The string key in the Map being the long name
  * supplied by the user. This key will include the long switch
  * indication (e.g. "--quiet")
  *
  * Switch values are initially false. If the user states them, the
  * values become true. Reword the switch to reverse behaviour
  * ('noIcecream').
  *
  * Builds a simple help response and catches input errors.
  *
  * To callback on several simple methods, see 
  * common.CommandLineTargetParser.
  *
  * @param codeName the name of the class/object with the main method.
  * @param summary one line statement of the general intention of the
  * invokation.
  * @param usage the format of the base invocation.
  * @param notes extra detail for understanding the usage.
  * @param callback method to be called if the user input parses.
  */
final class CommandLineOptionParser(
  codeName: String,
  summary: String,
  usage: String,
  notes: String,
  credits : String,
  private val callback: (Map[String, Boolean], String) => Unit
)
    extends CommanLineParserBase[OptionEntryTarget](
  codeName,
      summary,
      usage,
      notes,
      credits
)
{

  protected var options = IndexedSeq[OptionEntryTarget](
    // Though unused, here so it will display in help.
    new OptionEntryTarget("--help", "-h","display this help and exit", false)
  )

  private def getValues() : Map[String, Boolean] = options.map(oe => (oe.long, oe.value)).toMap


  protected def helpPrompt
  {
    System.err.println(s"Try `$usage --help' for more information.")
  }

  /** Add a switch to the parser.
    *
    * Switch indications '--' and '-' are added automatically.
    *
    * @param long long switch
    * @param short short switch
    * @param description typically start with a verb, and do
    *  *not* start with a capital or end with a period.
    */
  def += (
    long: String,
    short: String,
    description: String
  )
  {
    //TODO: test it doesn't exist?
    val opt = new OptionEntryTarget(
      "--" + long,
      "-" + short,
      description,
      false
    )
    options = opt +: options
  }

  /** Validates a list of data against the options.
    *
    * @return The first option that will not validate,
    *  if validation ok, None.
    */
  private def optionsRecurse(list: List[String])
      : Option[String] =
  {
    list match {
      case Nil => None
      case option :: tail => {
        val declared = options.find{opt =>
          ((opt.long == option) || (opt.short == option))
        }
        declared match {
          case None => Some(option)
          case Some(option) =>  {
            option.value = true
            optionsRecurse(tail)
          }
        }
      }
    }
  }

  def parse(args: Array[String])
  {
    // An inconsistent format, caught here
    if (args.length > 0 && args.contains("--help")) {
      printHelp
    }
    else {
      if (args.length == 0 || args.last.charAt(0) == '-') {
        System.err.println(s"$codeName: missing argument <file>")
        helpPrompt
      }
      else {

        val value = args.last
        val switches = args.slice(0, args.size - 1).toList
        if (switches.isEmpty) {
          callback(getValues, args(0))
        }
        else {

          optionsRecurse(switches) match {
            case Some(option) => {
              System.err.println(s"$codeName: invalid argument -- '$option'")
              helpPrompt
            }
            case None => {
              val v = getValues
              callback(v, value)
            }
          }
        }
      }
    }
  }

}//CommandLineOptionParser



object CommandLineOptionParser
{

  def apply(
    codeName: String,
    summary: String,
    usage: String,
    notes: String,
    credits : String,
    callback: (Map[String, Boolean], String) => Unit
  )
      : CommandLineOptionParser =
  {
    new CommandLineOptionParser(codeName, summary, usage, notes, credits, callback
    )
  }

}//CommandLineOptionParser
