package converter



/** Print to stdout, formatted and consistent.
  * 
  * Probably best to keep them central, too. We don't know the stdout
  * implementation.
  */
// It should be a trait, but it's causing instanciation grief
// ..and it's got data in it.
object Trace
    extends scala.io.AnsiColor
{




  private val newLine = System.lineSeparator()

  private val out = java.lang.System.out
  private val err = java.lang.System.err



  /** Write information output to standard output stream.
    *
    * Only if verbose is on.
    */
  def info(line: String)
  {
    out.println(GREEN + line + RESET)
  }


  /** Write information output to standard output stream.
    *
    * No newline. May need a `flush` when complete.
    */
  def infoPrint(str: String)
  {
    out.print(GREEN + str + RESET)
  }


  /** Flush output
    *
    * Sometimes necessary when simply printing, or print may not
    * become visible.
    */
  def flush() { out.flush() }


  /** Write information output to standard output stream.
    *
    * No newline. *Always outputs*. Intended for commandlines and
    * similar activity.
    */
  def terminalPrompt(prompt: String)
  {
    out.print(BOLD + BLUE + prompt + RESET)
    flush()
  }


  /** Write advice output to standard output stream.
    *
    * Only if verbose is on.
    */
  def advice(line: String)
  {
    out.println(CYAN + line + RESET)
  }


  /** Write warning output to standard error stream.
    *
    * Always prints.
    */
  def warning(line: String)
  {
    err.println(YELLOW + line + WHITE)
  }


  /** Write error output to standard error stream.
    *
    * Always prints.
    */
  def error(line: String)
  {
    err.println(RED + "** Error ** " + WHITE + line)
  }

  def critical(msg: String, e: Exception) {
    terminate(s"$msg on exception:\n${e.getMessage}")
  }

  def terminate(msg: String) {
    error(s"$msg")
    System.exit(1)
  }


  /** Write plain output to standard output stream
    * 
    * This method always prints, in the stock terminal font/color
    * settings. Used for output of the results of actions.
    */
  def plain(line: String)
  {
    out.println(line)
  }

}//Trace
