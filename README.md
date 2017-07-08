[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)

Gwen
====

Gwen is a [Gherkin](https://github.com/cucumber/cucumber/wiki/Gherkin) interpreter that turns Given-When-Then
steps into automation instructions and executes them for you so you don't have to do all the programming work. It has
an abstracted [evaluation engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines) allowing any type of
automation capability to be mixed in and reused.
[Meta specifications](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features) (also expressed in Gherkin) are used
to capture automation bindings and allow you to compose step definitions by mapping 'declarative' steps in features to
'imperative' steps in
[@StepDef scenarios](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features#composable-step-definitions)
that perform operations through pre-programmed automation engines.

### Current Status

[![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)

- [Latest release](https://github.com/gwen-interpreter/gwen/releases/latest)
- [Change log](CHANGELOG)

Why Gwen?
---------

> Gwen = [G]iven [W]hen Th[en]

Most Gherkin tools and frameworks target BDD (Behavior Driven Development) and therefore adopt a very developer-centric
or programmatic approach to verifying the behavior of code and driving its development. We wanted to build a BDA
(Behavior Driven Automation) tool that could use Gherkin to automate acceptance testing and robotic processing instead
with only little or no coding at all required on behalf of the user. So we built the Gwen interpreter to read Gherkin as input and produce executing instructions as output through automation engines that can be prebuilt with specific capabilities
and mixed in.

What engines are available?
---------------------------

- [gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - Our first engine is the gwen-web engine that we built on top of Selenium. You can download and use it to do web automation without having to develop any code to interact with Selenium.


Links
-----

- [Blog](https://gweninterpreter.wordpress.com)
- [Wiki](https://github.com/gwen-interpreter/gwen/wiki)
- [FAQ](https://github.com/gwen-interpreter/gwen/wiki/FAQ)

Core Runtime Requirement
------------------------

- Java SE 8 Runtime Environment

Key Features
------------

- [Gherkin grammar](https://cucumber.io/docs/reference)
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

Mail Group
----------

All announcements and discussions are posted and broadcast to all members in the following mail group. You are welcome to visit and subscribe to receive notifications or get involved.

- [Our mail group](https://groups.google.com/d/forum/gwen-interpreter)

Credits
-------
- Cucumber/Gherkin
- Scopt
- JLine
- Bootstrap
- JQuery
- Reel

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

