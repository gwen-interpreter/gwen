[![Gwen](https://github.com/gwen-interpreter/gwen/wiki/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)

Gwen
====

Gwen is a [Gherkin](https://cucumber.io/docs/gherkin/reference/) interpreter for driving automation with feature specifications. Automation is achieved through Gherkin bindings called meta specs which you compose with the Gwen DSL and maintain alongside your feature files. An embedded [web engine](https://github.com/gwen-interpreter/gwen-web) executes each step in your features according to the meta to automate operations in browsers for you.
 
Get Started
-----------

Visit the [Gwen home page](https://gweninterpreter.org) for our user documentation and getting started guide.

### Current Status

[![CI](https://github.com/gwen-interpreter/gwen/actions/workflows/ci.yml/badge.svg)](https://github.com/gwen-interpreter/gwen/actions/workflows/ci.yml)

- [Latest release](https://github.com/gwen-interpreter/gwen/releases/latest)
- [Change log](CHANGELOG)

### What's New?

- [Doc Strings as Parameters](https://github.com/gwen-interpreter/gwen/wiki/Doc-Strings/_edit#doc-strings-as-parameters)
- [Report Portal integration](https://github.com/gwen-interpreter/gwen/wiki/Report-Portal-Integration) for centralised reporting and real-time analytics.
- [Masked Settings](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#masked-settings) to make all your private and sensitive settings appear as `●●●●●` in all Gwen logs, reports, errors and console outputs

System Requirements
-------------------

- Java 8 (version 1.8) or higher

Key Features
------------

- Integrated [Gherkin](https://cucumber.io/docs/reference) parser from Cucubmer
- [Runtime settings](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings)
- [Evaluation engines](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines)
- [Evaluation reports](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Reports)
- [Command line interface](https://github.com/gwen-interpreter/gwen/wiki/Command-Line-Interface)
- [REPL console](https://github.com/gwen-interpreter/gwen/wiki/REPL-Console)
- [Serial execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#serial-execution)
- [Parallel execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#parallel-execution) for features or scenarios
- [Dry run execution and validation](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#dry-run-validation)
- [Data driven execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#data-driven-execution)
- [Meta features](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features)
- [Composable step definitions](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features#composable-step-definitions)
- [Runtime settings](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings)
- [User settings](https://github.com/gwen-interpreter/gwen/wiki/User-Settings)
- [String interpolation](https://github.com/gwen-interpreter/gwen/wiki/String-Interpolation)
- [SQL data bindings](https://github.com/gwen-interpreter/gwen/wiki/SQL-Data-Bindings)
- [Implicit attributes](https://github.com/gwen-interpreter/gwen/wiki/Implicit-Attributes)
- [Template matching](https://github.com/gwen-interpreter/gwen/wiki/Template-Matching)
- Hard, soft, and sustained [Assertion modes](https://github.com/gwen-interpreter/gwen/wiki/Assertion-Modes)
- [Synchronized StepDef execution](https://github.com/gwen-interpreter/gwen/wiki/Synchronized-StepDefs)
- Hard, soft, and sustained [Assertion modes](https://github.com/gwen-interpreter/gwen/wiki/Assertion-Modes)
- Supports full Gherkin syntax including [example mapping](https://cucumber.io/blog/2015/12/08/example-mapping-introduction)
- [State levels](https://github.com/gwen-interpreter/gwen/wiki/State-Levels) and [parallel execution](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes#parallel-scenario-execution) for scenarios in additon to features
- [Declarative feature mode](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#gwenfeaturemode) to force all imperative steps to meta and promote cleaner features.
- [Associative meta](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#gwenassociativemeta)
- [Behavior rules](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#gwenbehaviorrules) to help enforce good Gherkin style
- [Dialects](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#gwenfeaturedialect) for [Gherkin's spoken languages](https://cucumber.io/docs/gherkin/reference/#spoken-languages)
- Configurable [maximum number of threads](https://github.com/gwen-interpreter/gwen/wiki/Runtime-Settings#gwenparallelmaxthreads) for parallel execution
- Simplified [data table iteration with @ForEach](https://github.com/gwen-interpreter/gwen/wiki/Data-Tables#simplified-foreach)

License
-------

Copyright 2014-2021 Branko Juric, Brady Wood and [Gwen contributors](#code-contributors).

This software is open sourced under the
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE)

This project has dependencies on other open source projects. All distributed third party dependencies and their
licenses are listed in the [LICENSE-THIRDPARTY](LICENSE-THIRDPARTY) file.

Open sourced 14 May 2014 09:30 pm AEST

Contributions
-------------

New capabilities, improvements, and fixes and documentation are all valid candidates for contribution. Submissions can be made using pull requests. Each submission is reviewed and verified by the project [maintainers](#maintainers) before being integrated and released 
to the community. We ask that all code submissions include unit tests.

By submitting contributions, you agree to release your work under the [license](#license) that covers this software.

### How to Contribute

1. Fork this repository
2. Create a branch on your forked repository
3. Commit your changes to your branch
4. Push your branch to your forked repository
5. Create a pull request from your branch to here

Maintainers
-----------

- [Branko Juric](https://github.com/bjuric)
- [Brady Wood](https://github.com/bradywood)

Contributors
------------

We thank the following contributors and active users for helping to make Gwen better. You are all awesome!

### Code Contributors

The following [contributors](https://github.com/gwen-interpreter/gwen/graphs/contributors) submitted pull requests
that have been merged:

- [Jacob Juric](https://github.com/TheReturningVoid)
| [Alexandru Cuciureanu](https://github.com/acuciureanu)

### Active Users

The following users raised issues or requests that have been addressed:

- [Chris Leong](https://github.com/aztheque)
| [Martino Turturiello](https://github.com/martino-jelli)
| [bliddicoat](https://github.com/bliddicoat)
| [Andrew Gillett](https://github.com/asgillett)
| [anshu781126](https://github.com/anshu781126)
| [ketu4u2010](https://github.com/ketu4u2010)
| [Rahul9844](https://github.com/Rahul9844)
| [rkevin99](https://github.com/rkevin99)
| [dfriquet](https://github.com/dfriquet)

Credits
-------
- [Cucumber/Gherkin](https://docs.cucumber.io/gherkin/reference/)
- [Scopt](https://github.com/scopt/scopt)
- [JLine](https://github.com/jline/jline2)
- [Bootstrap](https://getbootstrap.com/)
- [JQuery](https://jquery.com/)
- [Reel](https://github.com/pisi/Reel)
