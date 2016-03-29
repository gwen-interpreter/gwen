[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
Gwen
====

Gwen is a [Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter that accepts plain text 
[features](https://github.com/cucumber/cucumber/wiki/Feature-Introduction) as input and produces automation instructions as output. It has an abstracted [evaluation engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines) allowing any type of automation to be mixed in.

> Gwen is a [G]iven [W]hen Th[en] automation platform

- See also: 
  - [Wiki](https://github.com/gwen-interpreter/gwen/wiki) 
  - [FAQ](https://github.com/gwen-interpreter/gwen/wiki/FAQ)
  - [Blog](https://gweninterpreter.wordpress.com)

### Current Status

[![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)

- Current release: [1.1.2](https://github.com/gwen-interpreter/gwen/releases/tag/v1.1.2) 
- [Change log](CHANGELOG)

### Automation by Interpretation

```gherkin    
   Feature: Gwen Interpreter
    
  Scenario: Automate
      Given a Gherkin feature
        And a target system
       When Gwen is launched
       Then the feature is interpreted
        And its steps are executed
        And the system is evaluated
```

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
- [Command line interface](https://github.com/gwen-interpreter/gwen/wiki/Command-Line-Interface)
- [REPL console](https://github.com/gwen-interpreter/gwen/wiki/REPL-Console)
- [Serial execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#serial-execution)
- [Parallel execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#parallel-execution)
  - with optional ramp up interval
- [Dry run execution and validation](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#dry-run-validation)
- [Data driven execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#data-driven-execution)
- [Meta features](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features)
- [Composable step definitions](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features#composable-step-definitions)
- [Runtime settings](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings)
- [User settings](https://github.com/gwen-interpreter/gwen/wiki/User-Settings)
- [String interpolation](https://github.com/gwen-interpreter/gwen/wiki/String-Interpolation)

Engines
-------

We have developed and shared the following engine that you can download and start using straight away!

- [Gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - A Gwen [engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines) that 
maps Gherkin features to [Selenium WebDriver](http://www.seleniumhq.org/projects/webdriver) 
instructions to automate web pages and web 
application testing

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

