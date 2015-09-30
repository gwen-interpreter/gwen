gwen FAQ
========

What are the batch and REPL modes?
----------------------------------
Batch mode forces gwen to immediately exit after a given feature (or features) are executed. 
You should always invoke gwen in batch mode in server environments (like build servers for example).
To execute gwen in batch mode, simply specify the `-b` or `--batch` command line switch.
```
bin/gwen -b features
```
or
```
bin/gwen --batch features
```

REPL (Read-Eval-Print-Loop) mode leaves the interpreter session open after a given feature 
(or features) are executed, allowing you to enter additional steps to execute and inspect the 
currently bound data in memory interactively at the gwen prompt. You should use this mode when 
you want to verify the execution of your features as you write them or when you want to 
diagnose why a feature is failing. Gwen runs in this mode by default (in the absence of the 
`-b` or `--batch` switch).

Can I run gwen with Java 8?
---------------------------

Yes.

How do I run a single feature file?
-----------------------------------
To run the `features/floodio/FloodIO.feature` file:
```
bin/gwen features/floodio/FloodIO.feature
```

How do I run all features files in a directory?
-----------------------------------------------
To run all feature files in the `features` directory (including all feature files in all sub-directories):
```
bin/gwen features
```

To run all feature files in the `features/floodio` directory (including all feature files in all sub-directories):
```
bin/gwen features/floodio
```

How do I run multiple features in sequence?
-------------------------------------------
To run multiple feature files or directories (feature suites) in sequence:
```
bin/gwen <feature-file-or-dir-1> <feature-file-or-dir-2> .. <feature-file-or-dir-N>
```

How do I run multiple features in parallel?
-------------------------------------------
To run multiple feature files or directories (feature suites) in parallel:
```
bin/gwen -| <feature-file-or-dir-1> <feature-file-or-dir-2> .. <feature-file-or-dir-N>
```
or 
```
bin/gwen --parallel <feature-file-or-dir-1> <feature-file-or-dir-2> .. <feature-file-or-dir-N>
```

How do I generate evaluation reports?
-------------------------------------

Specify the `-r` or `--report` option followed by the directory where you 
would like the reports to be generated.

For example, to generate the reports to a relative directory named `reports`:
```
bin/gwen -r reports
```
or
```
bin/gwen --report reports
```

How do I configure logging?
---------------------------

Gwen uses log4j to perform all logging using the configuration shown below 
(this default configuration is defined in the log4j.properties file bundled 
in the gwen binary). To override this and specify your own configuration, 
add a `log4j.configuration=file:/path-to-log4j-config-file` entry to your 
`gwen.properties` file in your user home directory. For more information 
about log4j, see [the log4j FAQ](http://logging.apache.org/log4j/1.2/faq.html).

```
# Set root logger level to INFO and append to STDOUT
log4j.rootLogger=INFO, STDOUT

# STDOUT is set to be a ConsoleAppender.
log4j.appender.STDOUT=org.apache.log4j.ConsoleAppender

# STDOUT uses PatternLayout.
log4j.appender.STDOUT.layout=org.apache.log4j.PatternLayout
log4j.appender.STDOUT.layout.ConversionPattern=%p - %m%n

# Gwen logging level
log4j.logger.gwen=INFO
```
