
==========================
A C++ to Scala converter
==========================

...kidding.

Once I knew this would take 6 months of my time, would need paying for, and was near-impossible, it became easier. I stuck to the possible.

The converter is unlikely to produce working code, but may move a project forward.


What it is
===========
A Parboiled parser builds an AST tree. The code then runs a phase on the tree, and the results are run into a Scala printer (other printers would be possible).


It parses C++?
--------------
Imagine the cost... no.


About the crazy 
====================

Slack parsing
--------------
The parser might be called relaxed. If the parser cannot parse, it passes the code, commenting it. It's like your bonkers co-worker, who piles through the intray shoving aside unmanageable tasks. And the parser makes multiple efforts to re-start and parse, even if it has lost track of the overall scheme.

This is an unusual take on parsing, and I don't know what the pros do, but is fundamental. The converter is here to help with grunt work, so it will try what it can, even if the results are mangled.

For example, if you feed the converter something else (I tried OCAML) it will comment everything except weird cross-language coincidences. The aim is, the internal parser should never fail.


Namespaces and classes
-----------------------
Namespaces... the intentions of C++ namespaces will not translate to Scala, so the converter comments them. Unless the namespace has no label/identifier --- then it is dropped.

Class heading code is mostly handled. Constructors and destructors are commented (constructor may be handled sometimes, but not now). The parser recognises basic templates (complexity will throw it) and the converter produces generics.


AST manipulation
-------------------
A compiler 'phase', but there is only one.

Tree code redistributes privacy markings over methods and fields.

Tree code can also construct a complementary object.

...and the tree code will surround namespaced objects with commented namespace marks (while dropping inline namespacing - see down a bit). This can be erratic in formatting, and making a C++ converter write C++ code is a joke. The author must like jokes (however, this option is default and useful, especially on `.cpp` files collecting namespaced functions/methods).


Methods
---------
Basic templates are sometimes recognised. The parser will heave through attributes parameters and body declarations. It will fail on the complex.

Most attributes are dropped. Recognised templating becomes generic. Types are switched to tail positions and rewritten.


Fields
-------
...or the many names C++ calls them. These are slashed from anything after them, e.g. assigning expressions, then prefixed with 'var' and the types converted.


Type conversion
------------------
If the parser succeeds on code in method or field declarations, the converter must convert C++ types to Java types. Most types dumb-translate --- ``double`` becomes `Double`. 

But ``char`` is commented --- it may be a byte or ASCII. In combined declarations such as ``long long int``, if more than one reference is made to ``long``, then the converter writes a ``Long``, otherwise ``Int``. Crude --- but so is oil.


Code block contents
--------------------
Parsing gets looser inside blocks. Within method code, it's uninvolved, making no effort to parse expressions at all.

However, will try to catch simple ``for`` loops, at the start of lines, turning them into ``while`` loops (Scala's ``for`` loop is rather different). Bear in mind a C++ program can be, ::

    for(initialization; test; program){ }; exit(0);

Don't expect too much.

It also. because Scala hasn't get them. can parse postfixed decrements and increments,


Formatting
----------
Formatting may wander. The stubborn attempt to parse code, come what may, means the converter can go adrift. A good indication code did not parse --- that's the author's excuse.

Formatting is not like the original, since parse results are used to build an AST tree, so the converter output is a rebuild. However, the converter output seems to be readable alongside the original (important).


Building
===========
On Linux, run, ::

    ./make_compile_and_jar

Then configure the ``CPPToScala`` script with the path to your compiled folders and Scala installation. Copy this shell file wherever you'd like to run ``CPPToScala``, and, ::

    ./CCPToScala <filepath>

or, ::

    ./CCPToScala --help 



Commandline
==============

:--makeComplementaryObjects:
  make a complementary object. Constructors, if successfully parsed, are turned into factory objects.
:--commentUnparsedLines:
  put a comment on lines where the parsing gave up. Since this is 2/3 of a conversion, the output can be tedious at length, but the switch results help spot places parsing failed.
:--verticalParams:
  arrange method parameters vertically. Not a common code style, but helps the visibility of conversion issues.
:--tabBy2:
  only tab by 2 spaces, not 4 (2 is common in Scala, but 4 is easier to read and compare to C++)
:--noNamespacing:
  do not place namespace indications on namespaced code. Less eccentric, but the indications help.


Overall
============
The converter will have a go at converting toplevel code and namespacing. It catches some methods and fields. It has only occasional stabs at code in blocks, mainly C++ expressions which will not work at all in Scala.

Expect it to make working code and you will be disappointed. It can't parse ``Hello world.cpp``. Ask it to do some grunt work, and it may help.

Last thoughts
================
...and it should be written in OCAML or Haskell or something. It would launch faster. But Scala is way easier to handle.


References
==========
Don't blame him, my fault,
  https://github.com/sirthias/parboiled/wiki

