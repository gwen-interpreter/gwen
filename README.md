[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
Gwen
====

Gwen is a [Gherkin](https://github.com/cucumber/cucumber/wiki/Gherkin) interpreter that turns
[Given-When-Then](https://github.com/cucumber/cucumber/wiki/Given-When-Then) steps into automation instructions and
executes them for you so you don't have to do all the programming work. It has an abstracted
[evaluation engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines)
allowing any type of automation capability to be mixed in and reused. For example, we have built and
shared a [web engine](https://github.com/gwen-interpreter/gwen-web) that you can download and use to automate web
application testing and robotic web activities.
[Meta specifications](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features) (also expressed in Gherkin) can be
used to capture automation bindings and map 'declarative' feature steps to 'imperative' engine steps that perform operations.

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
- [SQL data bindings](https://github.com/gwen-interpreter/gwen/wiki/SQL-Data-Bindings)

Runtime Requirements
--------------------

- Java SE 8 Runtime Environment

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

How to contribute:
1. Fork this repository
2. Create a branch on your forked repository
3. Commit your changes to your branch
4. Push your branch to your forked repository
5. Create a pull request from your branch to here

License
-------

Copyright 2014-2017 Branko Juric, Brady Wood

This software is open sourced under the 
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE)

This project has dependencies on other open source projects. All distributed third party depdendencies and their licenses are listed in the 
[LICENSE-THIRDPARTY](LICENSE-THIRDPARTY) file.

Open sourced 14 May 2014 09:30 pm AEST

