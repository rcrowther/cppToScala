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
  * This parser calls back on a method, and sends processed details to
  * that method as x(options:Map[String, Boolean],
  * arg:Seq[String]). The string key in the Map is the long switch
  * name supplied by the user. This key will include the long switch
  * indication (e.g. "--quiet"). If the user supplies short switches,
  * these are turned into the long version.
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
  * @param usage the format of the base invocation - usually the program name.
  * @param notes extra detail for understanding the usage.
  * @param callback method to be called if the user input parses.
  */
final class CommandLineOptionParser(
  codeName: String,
  summary: String,
  usage: String,
  notes: String,
  version : String,
  credits : String,
  private val callback: (Map[String, Boolean], Seq[String]) => Unit
)
    extends CommanLineParserBase[OptionEntryTarget](
  codeName,
      summary,
      usage,
      notes,
      version,
      credits
)
{

  protected var options = IndexedSeq[OptionEntryTarget](
    // Though unused, here so it will display in help.
    new OptionEntryTarget("--help", "-h","display this help and exit", false),
    new OptionEntryTarget("--version", "-vsn","display version information and exit", false)
  )

  private def getSwitches() : Map[String, Boolean] = options.map(o => (o.long, o.value)).toMap


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

    if (args.length == 0) {
      System.err.println(s"$codeName: missing arguments")
      helpPrompt
    }
    else {

      // Split switches from values
      var splitPoint = 0
      var cont = true
      val limit = args.size
      do {
        if (args(splitPoint).charAt(0) == '-') splitPoint += 1
        else cont = false
      } while(splitPoint < limit && cont)

      val (switches, values) = args.splitAt(splitPoint)

      // Check no switches in values
      val falseValues = values.filter(_.charAt(0) == '-')
      if (!falseValues.isEmpty) {
        System.err.println(s"$codeName: switch(es) in value position: ${falseValues.mkString(" ")}")
      }
      else {

        if (switches.isEmpty) {
          callback(getSwitches, values)
        }
        else {

          optionsRecurse(switches.toList) match {
            case Some(option) => {
              System.err.println(s"$codeName: invalid argument -- '$option'")
              helpPrompt
            }
            case None => {
              val processedSwitches = getSwitches
              // Inconsistent formats and immediacies, caught here
              if (
                processedSwitches("--help")
                  || processedSwitches("--version")
              )
              {
                if (processedSwitches("--help")) printHelp
                else printVersion
              }
              else {
                callback(processedSwitches, values)
              }
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
    version : String,
    credits : String,
    callback: (Map[String, Boolean], Seq[String]) => Unit
  )
      : CommandLineOptionParser =
  {
    new CommandLineOptionParser(
      codeName,
      summary,
      usage,
      notes,
      version,
      credits,
      callback
    )
  }

}//CommandLineOptionParser
