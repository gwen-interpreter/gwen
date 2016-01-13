[![gwen](https://github.com/gwen-interpreter/gwen/blob/master/doc/img/gwen-attractor.png)](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo)
Gwen
====

Gwen is a 
[Gherkin DSL](https://github.com/cucumber/cucumber/wiki/Gherkin) 
interpreter that accepts 
[feature specifications](https://github.com/cucumber/cucumber/wiki/Feature-Introduction) 
as input and produces automation instructions as output. It has an abstracted 
evaluation engine allowing any type of automation to be mixed in.

> Gwen is a [G]iven [W]hen Th[en] automation platform

- See also [Gwen wiki](https://github.com/gwen-interpreter/gwen/wiki)

### Current Status

[![Build Status](https://travis-ci.org/gwen-interpreter/gwen.svg)](https://travis-ci.org/gwen-interpreter/gwen)

### Automation by Interpretation

```gherkin    
   Feature: Gwen Interpreter
    
  Scenario: Automate
      Given a Gherkin feature
        And a target system
       When Gwen is launched
       Then the feature is interpreted
        And the steps are executed
        And the system is evaluated
```

The gwen interpreter reads Gherkin features and parses them into an abstract 
syntax tree for validation and correctness. It then traverses all scenarios 
and dispatches the processing of each step to a mixed in evaluation engine. 
The engine performs the automation work on the target system. Gwen then 
captures and reports the results.

Internally, Gwen uses the [Gherkin 3](https://github.com/cucumber/gherkin3) 
parser to read in features.

- See [Supported Grammar](https://github.com/gwen-interpreter/gwen/wiki/Supported-Grammar)


Key Features
------------

- [Gherkin feature parser](https://github.com/gwen-interpreter/gwen/wiki/Supported-Grammar)
- [Abstracted evaluation engine](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines)
- [Evaluation reports](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Reports)
- [Execution Modes](https://github.com/gwen-interpreter/gwen/wiki/Execution-Modes)
  - [REPL console](https://github.com/gwen-interpreter/gwen/wiki/REPL-Console)
  - Batch
    - Serial 
    - Parallel (`--parallel`)
  - Dry Run (`--dry-run` validation mode)
  - Data driven (csv data feeds)
- [Meta features](https://github.com/gwen-interpreter/gwen/wiki/Meta-Features)
  - Composable step definitions (@StepDef's)
- Tagged features and scenarios
- See also: [CHANGELOG](CHANGELOG)

Core Requirements
-----------------

- Java SDK 7+
- Scala 2.11.x, 2.10.x (for dev environment only)

Evaluation Engines
------------------

### Web engine
We have developed and shared the following web engine that you can download 
and start using straight away:

- [gwen-web](https://github.com/gwen-interpreter/gwen-web)
  - A Gwen engine that maps Gherkin features to [Selenium WebDriver](http://www.seleniumhq.org/projects/webdriver) 
    instructions to automate web pages and web application testing through 
    specifications instead of code

### Many other engines can be built
- Read more about [evaluation engines](https://github.com/gwen-interpreter/gwen/wiki/Evaluation-Engines).

***

Development
-----------

- See [Development Wiki](https://github.com/gwen-interpreter/gwen/wiki/Development) 

FAQ
---

- See [Gwen FAQ](https://github.com/gwen-interpreter/gwen/wiki/FAQ)

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

Copyright 2014-2016 Branko Juric, Brady Wood

This software is open sourced under the 
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.txt).

See also: [LICENSE](LICENSE).

This project has dependencies on other open source projects. All distributed 
third party depdendencies and their licenses are listed in the 
[LICENSE-Third-Party.txt](LICENSE-Third-Party.txt) file.


Open sourced 14 May 2014 09:30 pm AEST

Mentions
--------

- 2013-2014, [Mick Grigg](http://au.linkedin.com/in/mickgrigg) for 
  involving us in your side project and supporting us in open sourcing this 
  interpreter which we built as part of that. 
- 2014, [Arun Datta](http://au.linkedin.com/in/arundatta) for reviewing our 
  pre-release documentation and providing valuable feedback.
- 2014, [Nascent Creative](http://www.nascentcreative.com.au) for developing 
  the [project logo](https://github.com/gwen-interpreter/gwen/wiki/The-Gwen-Logo).
