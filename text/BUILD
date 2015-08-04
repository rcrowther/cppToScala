
*Do not read the comments below the line*
Live in hope, but they do not work currently. Creating an executable file in one with Scala and Parboiled is not a problem I have fixed.

For now...


== Dependancies
Need an installed Java, and a Scala somewhere. The Scala must be in a known place, not packaged someplace it can't be accessed.

CPPToScala needs a version of Parboiled, the Java/Scala PEG parser. However, this should come bundled in the /lib folder.
 

== Build
Run the script. It will not make a .jar, but run it.

./make_compile_and_jar

The results are very verbose. You should end up with a build folder.


== Everyone else
Follow the directions in the shell file, and manually compile.


== Run
Go into the script CCPToScala, and add the addreess of your Scala installation to SCALA_HOME. Add the adress of the compile folder to SCRIPT_HOME.

Now copy and the script anywhere you want to run ccpToScala, and,

./CCPToScala <filepath>

or,

./CCPToScala --help








///////////////////////////
== Build and run
The way to use ccpToScala is to make a .jar. See below.

If it can be built ok, put the .jar where code needs converting. Then,

scala ccpToJava.jar <name a file>

or

scala ccpToJava.jar --help
 



== Linux build - use the script

The script MUST have a Scala folder targeted. Installed versions will not work. The script will make a half-hearted attempt to find a folder in /bin/, but it may not work. Set SCALA_HOME in the script, to be sure.

The reason is, the executable jar must have the Scala library installed to run, so it copies it from the installation folder. So aliases/installed versions will not provide the information needed to get the library.

./make_compile_and_jar

The results are very verbose. You should end up with a .jar


== Everyone else
You could follow the directions in the shell file, and manually compile. It's something like,

Build:
scalac -d build -toolcp lib/parboiled-scala_2.10-1.1.7.jar -toolcp lib/parboiled-core-1.1.7.jar src/*

Jar:

Create MANIFEST.MF file containing:
"Manifest-Version: 1.0
Specification-Title: cppToScala
Main-Class: converter.CPPToScala
"

Then jar:
jar cfm cppToScala.jar MANIFEST.MF -C build . 


