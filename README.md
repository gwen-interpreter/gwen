[![gwen](https://github.com/gwen-interpreter/gwen/blob/master/doc/img/gwen-attractor.png)](doc/LOGO.md)

gwen [![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)
====

Gwen is a 
[Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter that accepts 
[feature specifications](https://github.com/cucumber/cucumber/wiki/Feature-Introduction) 
as input and produces automated processes as output. It has an abstracted 
evaluation engine allowing any type of automation to be mixed in. 

> Gwen = [G]iven [W]hen Th[en]

Evaluation Engines
------------------

Evaluation engines map incoming 
[steps](https://github.com/cucumber/cucumber/wiki/Given-When-Then) to 
functions and operations on target systems. Automation is achieved by:  

- Creating conditions (_Givens_)
- Performing actions (_Whens_)
- Asserting expectations (_Thens_)

Any custom or public engine implementation can run inside the interpreter. 
Once the interpreter has been loaded with an engine, it can readily execute 
both individual steps and complete feature files. We have developed and 
shared the following engine:

- [gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - A web engine for automating web application testing 
    through specifications instead of code.

If the above engine suits your automation needs, then you can download 
and start using it straight away. If on the other hand you would like to 
use gherkin features to drive a different type of automation then you can 
[develop a custom engine](doc/START.md) and mix it in.

Automation by Interpretation
----------------------------

```    
   Feature: Gwen Interpreter
    
  Scenario: Automate
      Given a specification
        And a target system
       When gwen is invoked
       Then the specification is interpreted
        And the steps are executed
        And the system is evaluated
```

The gwen interpreter reads Gherkin features and parses them into an abstract 
syntax tree for validation and correctness. It then traverses all scenarios 
and dispatches the processing of each step to a mixed in evaluation engine. 
The engine performs the automation work on the target system. Gwen then 
captures and reports the results.

Key Features
------------

- Gherkin feature parser
- Abstract evaluation engine
- REPL console
- Evaluation reports (Rich HTML and JUnit-XML)
- Data scopes
- Meta features
- Composable step definitions (@StepDef's)
- Serial and parallel execution
- Tagged features and scenarios
- Design time validation (--dry-run mode)
- Data driven execution (csv data feeds)

Core Requirements
-----------------

- Java SDK 8+
- Scala 2.11.x, 2.10.x 

Supported Grammar
-----------------

The following subset of the Gherkin grammar is supported (shown here in 
[EBNF](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) 
notation).

```
      
  spec        = feature, 
                [background], 
                {scenario}
  feature     = {tag}, 
                "Feature:", name
                [description]
  background  = "Background:", name
                [description]
                {step}
  scenario    = {tag}, 
                "Scenario:", name
                [description]
                {step}
  tag         = "@", name
  step        = keyword, expression
  keyword     = "Given" | "When" | "Then" | "And" | "But"
  name        = expression
  description   {expression}
  comment     = "#", expression
  expression  = character, {character}
 
```

Internally, Gwen uses the [Gherkin 3](https://github.com/cucumber/gherkin3) 
parser to read in Gherkin features.

REPL Console
------------

Gwen also provides a REPL console. This is a command line shell that prompts 
you for steps, evaluates them, and then prints their results. You can use it 
to evaluate individual steps or complete feature files. This gives you the 
ability to experiment and observe the execution of steps and features as you 
write them. The scoped data in memory can also be printed to the console at 
any time for debugging and analysis. The REPL also saves every step and 
command you type to a local history file. So you can use the up and down 
arrows to recall previous inputs. Help is also available by typing `help` 
(which lists all available commands and what they do). All of this makes the 
REPL a very powerful tool for authoring executable features and testing them.

Note that you cannot launch the interpreter without mixing in an engine. The 
following shows the REPL console for the sample _MathInterpreter_ that is 
included in the test source of this project and is the focus of our 
[getting started](#getting-started) guide.

![Gwen REPL Console](doc/img/gwen-repl.png)

Evaluation Reports
------------------

Gwen reports all evaluated results to the system output stream. Pretty HTML 
reports containing detailed results, statistics, and summaries can also be 
generated and written to the file system. JUnit XML reports can also 
be optionally generated for easy integration with most build servers.  

![Gwen Evaluation Report](doc/img/gwen-report.png)  

Data Scopes
-----------

Dynamically scoped attributes provide engines with a flexible means of binding 
data to memory. Your evaluation engines can make use of this facility to 
manage scoped data between steps and scenarios in the feature evaluation 
lifecycle.

Meta Features
-------------

In addition to standard Gherkin features, gwen also supports and introduces 
meta features. These are also defined in Gherkin and can provide powerful 
capabilities for eliminating redundancy. They also provide a clean separation 
between evaluation and configuration. Meta features are loaded into the 
interpreter on startup and can be used to initialise and configure the 
environment context for engines at load time.

Composable Steps
----------------

Composable step definitions allow you to declaratively compose custom steps 
in terms of other steps to create what are effectively callable procedures. 
They are declared exactly like scenarios but are annotated with the @StepDef 
tag and bind a sequence of steps to an expression that serves as the 
callable name. StepDefs can be defined in both standard features and meta 
features and are cached in the interpreter when loaded. They must be declared 
before any scenarios that use them, and are evaluated only when referenced by 
name in those scenarios or other StepDefs at runtime.

Execution Modes
---------------

Gwen can interpret single feature files and suites of feature files in a 
directory.  When a single feature file is passed to gwen, only that file 
is interpreted.  When a directory is passed to gwen, all feature files in the 
directory and its subdirectories are interpreted. For any given feature file, 
all existing meta files in the feature file's path are discovered and loaded 
first. Any number of files or directories can be passed to gwen in a 
single call for sequential or parallel execution. Tags can also be passed to 
include and exclude features and scenarios annotated with those tags.

### Data Driven Execution

CSV data files can also be passed into the interpreter to perform feature 
execution over multiple data sets. If a data file exists in a feature 
file's directory (or somewhere in it's path) it will be automatically 
discovered and used to execute the feature over each of its data records. 
Only one such CSV file is allowed in any feature path (a runtime error will 
be thrown if more than one anywhere in the path is found). The first row in 
a CSV data file must be a list of column names for the contained data that 
follows. The values in each record will be bound to attributes in the 
feature scope having these same names. Feature steps can reference the bound 
data using these names.   

***

Development
===========

Getting Started
---------------

See our [getting started](doc/START.md) guide for a quick introduction 
to the development environment and a short tutorial on how to build an 
evaluation engine and mix it into the interpreter.

Adding Gwen as a Binary Dependency
----------------------------------

To build a new engine, you will need to create a new scala project and include 
gwen as a dependency.

### SBT

To add gwen as a binary dependency in an sbt project, add the following line to 
your build.sbt file. The `%%` instructs sbt to pull 
down the packaged dependency that matches your `scalaVersion` setting. The 
`latest.integration` selects the latest published revision of gwen (you may 
wish to fix this to the resolved version after gwen has downloaded, or to a 
release version that you wish to target).

```
libraryDependencies += "org.gweninterpreter" %% "gwen" % "latest.integration"
```

Also add the following repositories:

```
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
```

Building from Source
--------------------

If you would like to build a binary from the source:

1. Download and install [Java 7+](http://www.oracle.com/technetwork/java/javase/downloads/index.html) 
2. Download and install the latest [sbt](http://www.scala-sbt.org/) version
3. Download a [Git client](http://git-scm.com/downloads) 
4. Clone the gwen source repository at https://github.com/gwen-interpreter/gwen 
5. Change to the directory where you cloned the gwen source
6. Run `sbt test` to compile and run all tests and verify that all is OK
7. Run `sbt package` to build a JAR file
   - This will create a _gwen-[version].jar_ file in the 
     _target/scala-[version]_ folder relative to your current directory

FAQ
---

> See [gwen FAQ](doc/FAQ.md)

***

Mail Group
----------

All announcements and discussions are posted and broadcast to all members of 
the following mail group. You are welcome to visit and subscribe to receive 
notifications or get involved.

- [gwen-interpreter](https://groups.google.com/d/forum/gwen-interpreter) 

Contributions
-------------

New capabilities, improvements, and fixes are all valid candidates for 
contribution. Submissions can be made using pull requests. Each submission 
is reviewed and verified by the project's committer's before being integrated 
and released to the community. We ask that all code submissions include unit 
tests.

By sending a pull request, we assume that you agree to release your work under 
the license that covers this software.

License
-------

Copyright 2014-2015 Branko Juric, Brady Wood

This software is open sourced under the 
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE).

This project has dependencies on other open source projects, all of which are 
listed in the [NOTICE](NOTICE) file.

Open sourced 14 May 2014 09:30 pm AEST

Mentions
--------

- 2013-2014, [Mick Grigg](http://au.linkedin.com/in/mickgrigg) for 
  involving us in your side project and supporting us in open sourcing this 
  interpreter which we built as part of that. 
- 2014, [Arun Datta](http://au.linkedin.com/in/arundatta) for reviewing our 
  pre-release documentation and providing valuable feedback.
- 2014, [Nascent Creative](http://www.nascentcreative.com.au) for developing 
  the [project logo](doc/LOGO.md).
