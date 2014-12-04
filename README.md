![Gwen](https://github.com/gwen-interpreter/gwen/blob/master/doc/img/gwen-attractor.png)

gwen-interpreter
================

> [G]iven-[W]hen-Th[en]

Gwen is a [Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter with an abstracted evaluation engine. The following implementation 
is currently available:

- [gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - A web engine for automating web application testing in a way that requires 
    no development on behalf of the user.

If you are a user who would like to start using gwen, then you can download 
and install the above implementation. If on the other hand you are a developer 
who would like to build a new engine implementation or are interested in a 
technical overview of the platform, then please read on.

Automation by Interpretation
----------------------------

```    
   Feature: Gwen Interpreter
    
  Scenario: Automation by interpretation
      Given a software system
        And a behavioral specification
       When Gwen is invoked
       Then the specification is interpreted
        And the behavior is evaluated
```

### How does it work?

The Gwen interpreter reads Gherkin features and parses them into an abstract 
syntax tree for validation and correctness. It then traverses all scenarios 
and dispatches the processing of each step to an evaluation engine that you 
define (or provide) and mix in. Your engine performs the actual evaluation 
work required for each step. Gwen then captures and reports the results.

Key Features
------------

- Gherkin feature parser
- Evaluation abstraction
- REPL console
- Evaluation reports
- Data scopes
- Meta features
- Composable steps
- Serial and parallel execution
- Tagged features and scenarios

Core Requirements
-----------------

- Java 1.6 or later
- Scala 2.11.x, 2.10.x 

Development
-----------

### Adding Gwen as a Binary Dependency

To build a new engine, you will need to create a new scala project and include 
Gwen as a dependency.

#### SBT

To add Gwen as a binary dependency in an sbt project, add the following line to 
your build.sbt file. The `%%` instructs sbt to pull 
down the packaged dependency that matches your `scalaVersion` setting. 

```
libraryDependencies += "org.gweninterpreter" %% "gwen" % "0.1.0-SNAPSHOT"
```

Also add the following repositories:

```
resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
```

#### Maven

If you are using a maven project, add the following dependency to your pom.xml 
file. In this case, you will need to explicitly specify the scala version in 
the `artifactId` suffix as shown.    

```
<dependency>
	<groupId>org.gweninterpreter</groupId>
	<artifactId>gwen_2.11</artifactId>       <!-- for scala 2.11.x -->
	<!--artifactId>gwen_2.10</artifactId-->  <!-- for scala 2.10.x -->
	<version>0.1.0-SNAPSHOT</version>
	<type>jar</type>
</dependency>
```

Also add the following repositories:

```
<repository>
	<id>typesafe-releases-repo</id>
	<name>Typesafe Releases Repo</name>
	<url>http://repo.typesafe.com/typesafe/releases/</url>
</repository>
<repository>
	<id>sonatype-snapshots-repo</id>
	<name>Sonatype Snapshots</name>
	<url>https://oss.sonatype.org/content/repositories/snapshots</url>
</repository>
```

### Building from Source

If you would like to build a binary from the source:

1. Download and install [Java SDK 1.6 or later](http://www.oracle.com/technetwork/java/javase/downloads/index.html) 
   - Note Java 8 is not recommended at this time of writing
2. Download and install the latest [sbt](http://www.scala-sbt.org/) version
3. Either
   - Download a [Git client](http://git-scm.com/downloads) and clone this 
     repository using one of the following (SSH or HTTPS) URLs: 
     - `git@github.com:gwen-interpreter/gwen.git`
     - `https://github.com/gwen-interpreter/gwen.git`
   - Or click the Download ZIP link on this GitHub project page to download 
     the source archive and extract it to a local folder 
4. Change to the directory where you cloned/downloaded the gwen source
5. Run `sbt test` to compile and run all tests and verify that all is OK
6. Run `sbt package` to build a JAR file
   - This will create a _gwen-[version].jar_ file in the 
     _target/scala-[version]_ folder relative to your current directory

### Getting Started

See our [getting started](doc/START.md) guide for a quick introduction 
to the development environment and a short tutorial on how to build an 
evaluation engine and mix it into the interpreter.

Supported Grammar
-----------------

The following subset of the Gherkin grammar is supported (shown here in 
[EBNF](http://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_Form) 
notation).

```
 {{{
      
  spec        = feature, 
                [background], 
                {scenario}
  feature     = {tag}, 
                "Feature:", name
                [narrative]
  narrative     "As a ", expression
                "I want ", expression
                ["So that ", expression]
  background  = "Background:", name
                {step}
  scenario    = {tag}, 
                "Scenario:", name
                {step}
  tag         = "@", name
  step        = keyword, expression
  keyword     = "Given" | "When" | "Then" | "And" | "But"
  expression  = character, {character}
  name        = {character}
  comment     = "#", expression
 
 }}}
```

Evaluation Engines
------------------

Gwen introduces the concept of reusable evaluation engines that can be 
mixed into the interpreter. Evaluation engines map incoming steps to 
functions and operations on target APIs to evaluate behavior. You prescribe 
what steps are supported and how they are processed by implementing your own 
engine using the abstractions provided. Once the interpreter has been loaded 
with your engine, it can readily execute both individual steps and complete 
features.

REPL Console
------------

Gwen also provides a REPL console. This is a command line shell that prompts 
you for steps, evaluates them, and then prints their results. You can use it 
to evaluate individual steps or complete feature files. This gives you the 
ability to experiment and observe the execution of steps and features as you 
write them. The scoped data in memory can also be printed to the console at 
any time for debugging and analysis. The REPL also saves every step and 
command you type to a local history file. So you can use the up and down 
arrows to recall previous inputs. All of this makes the REPL a very powerful 
tool for authoring executable features and testing them.

Note that you cannot launch the interpreter without mixing in an engine. The 
following shows the REPL console for the sample _MathInterpreter_ that is 
included in the test source of this project and is the focus of our 
[getting started](#getting-started) guide.

![Gwen REPL Console](doc/img/gwen-repl.png)

Evaluation Reports
------------------

Gwen reports all evaluated results to the system output stream. Pretty HTML 
reports containing detailed results, statistics, and summaries can also be 
generated and written to the file system.

![Gwen Evaluation Report](doc/img/gwen-report.png)  

Data Scopes
-----------

Dynamically scoped attributes provide engines with a flexible means of binding 
data to memory. Your evaluation engines can make use of this facility to 
manage scoped data between steps and scenarios in the feature evaluation 
lifecycle.

Meta Features
-------------

In addition to standard Gherkin features, Gwen also supports and introduces 
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
directory.  When a single feature file is passed to Gwen, only that file 
is interpreted.  When a directory is passed to Gwen, all feature files in the 
directory and its subdirectories are interpreted. For any given feature file, 
all existing meta files in the feature file's path are discovered and loaded 
first. Any number of files or directories can be passed to Gwen in a 
single call for sequential or parallel batch execution. Tags can also be 
passed to include and exclude features and scenarios annotated with those 
tags.

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

Copyright 2014 Branko Juric, Brady Wood

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
