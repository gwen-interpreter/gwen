[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
Gwen
====

Gwen is a [Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter that accepts plain text 
[features](https://github.com/cucumber/cucumber/wiki/Feature-Introduction) 
as input and produces automation instructions as output. It has an abstracted 
engine allowing any type of automation to be mixed in.

> Gwen is a [G]iven [W]hen Th[en] automation platform
 
### Current Status

[![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)

Core Requirements
-----------------

- Java 7+
- Scala 2.11.x, 2.10.x
 
Gwen is written in [Scala](http://www.scala-lang.org) and runs on 
[Java](https://www.oracle.com/java).

Key Features
------------

- [Gherkin grammar](https://github.com/gwen-interpreter/gwen/wiki/Supported-Grammar)
- [Evaluation engines](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines)
- [Evaluation reports](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Reports)
- [REPL console](https://github.com/gwen-interpreter/gwen/wiki/REPL-Console)
- Serial and parallel execution 
- Dry run validation
- CSV data feeds
- [Meta features](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features)
- Composable `@StepDef`s
- Tagged features and scenarios

Resources
---------

- [Wiki](https://github.com/gwen-interpreter/gwen/wiki)
  - [Development](https://github.com/gwen-interpreter/gwen/wiki/Development) 
  - [FAQ](https://github.com/gwen-interpreter/gwen/wiki/FAQ)
- [Change log](CHANGELOG)

Engines
-------

We have developed and shared the following engine that you can download and 
start using straight away!

- [Gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - An evaluation engine that maps Gherkin features to [Selenium WebDriver](http://www.seleniumhq.org/projects/webdriver) 
    instructions to automate online activities and web application testing

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
is reviewed and verified by the project committers before being integrated 
and released to the community. We ask that all code submissions include unit 
tests.

By submitting contributions, we assume that you agree to release your work under 
the license that covers this software.

License
-------

Copyright 2014-2016 Branko Juric, Brady Wood

This software is open sourced under the 
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE).

This project has dependencies on other open source projects. All distributed 
third party depdendencies and their licenses are listed in the 
[LICENSE-THIRDPARTY](LICENSE-THIRDPARTY) file.

Open sourced 14 May 2014 09:30 pm AEST

Mentions
--------

- 2013-2014, [Mick Grigg](http://au.linkedin.com/in/mickgrigg) for 
  involving us in your side project and supporting us in open sourcing this 
  interpreter which we built as part of that. 
- 2014, [Arun Datta](http://au.linkedin.com/in/arundatta) for reviewing our 
  pre-release documentation and providing valuable feedback.
- 2014, [Nascent Creative](http://www.nascentcreative.com.au) for developing 
  the [project logo](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
