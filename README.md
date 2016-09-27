[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
Gwen
====

Gwen is a Given-When-Then automation platform. It is a [Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter that accepts plain text 
[features](https://github.com/cucumber/cucumber/wiki/Feature-Introduction) as input and produces automation instructions as output. It has an abstracted [evaluation engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines) allowing any type of automation to be mixed in. We have developed and shared a [gwen-web](https://github.com/gwen-interpreter/gwen-web) engine that maps Gherkin features to [Selenium WebDriver](http://www.seleniumhq.org/projects/webdriver) instructions. You can use it to automate online activities and web application testing through plain text feature specifications instead of code.

### Current Status

[![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)

- [Latest release](https://github.com/gwen-interpreter/gwen/releases/latest) 
- [Change log](CHANGELOG)

Key Features
------------

- [Gherkin grammar](https://github.com/gwen-interpreter/gwen/wiki/Supported-Grammar)
- [Evaluation engines](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines)
- [Evaluation reports](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Reports)
- [Command line interface](https://github.com/gwen-interpreter/gwen/wiki/Command-Line-Interface)
- [REPL console](https://github.com/gwen-interpreter/gwen/wiki/REPL-Console)
- [Serial execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#serial-execution)
- [Parallel execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#parallel-execution) (with configurable ramp up interval)
- [Dry run execution and validation](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#dry-run-validation)
- [Data driven execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#data-driven-execution)
- [Meta features](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features)
- [Composable step definitions](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features#composable-step-definitions)
- [Runtime settings](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings)
- [User settings](https://github.com/gwen-interpreter/gwen/wiki/User-Settings)
- [String interpolation](https://github.com/gwen-interpreter/gwen/wiki/String-Interpolation)

Core Requirements
-----------------

- Java 7+
- Scala 2.11.x, 2.10.x
 
Gwen is written in [Scala](http://www.scala-lang.org) and runs on [Java](https://www.oracle.com/java).

Resources
---------

  - [Wiki](https://github.com/gwen-interpreter/gwen/wiki) 
  - [FAQ](https://github.com/gwen-interpreter/gwen/wiki/FAQ)
  - [Blog](https://gweninterpreter.wordpress.com)

Mail Group
----------

All announcements and discussions are posted and broadcast to all members in the following mail group. You are welcome to visit and subscribe to receive notifications or get involved.

- [gwen-interpreter](https://groups.google.com/d/forum/gwen-interpreter) 

Contributions
-------------

New capabilities, improvements, and fixes are all valid candidates for contribution. Submissions can be made using pull requests. Each submission 
is reviewed and verified by the project committers before being integrated and released to the community. We ask that all code submissions include unit tests.

By submitting contributions, you agree to release your work under the license that covers this software.

License
-------

Copyright 2014-2016 Branko Juric, Brady Wood

This software is open sourced under the 
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE)

This project has dependencies on other open source projects. All distributed third party depdendencies and their licenses are listed in the 
[LICENSE-THIRDPARTY](LICENSE-THIRDPARTY) file.

Open sourced 14 May 2014 09:30 pm AEST

