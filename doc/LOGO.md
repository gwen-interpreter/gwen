About the Gwen Logo
===================

![Gwen Logo](img/gwen-attractor.png)

The Gwen logo is a
[Lorenz attractor](http://en.m.wikipedia.org/wiki/Lorenz_attractor) / 
butterfly. 
    
```
   Feature: Deterministic Chaos
    
  Scenario: The butterfly effect
      Given a deterministic nonlinear system
       When a small change is initially applied
       Then a large change will eventually result
```
 
Dynamic systems can be observed to possess chaotic behavior and this is common 
in software. The smallest of changes to programs and environments can give 
rise to the most erratic effects. Gwen evaluates software systems by 
interpreting Gherkin features and dispatching their _Given-When-Then_ steps to 
evaluation engines that in turn map those to discrete units of work to create 
conditions (_Givens_), perform operations (_Whens_), and assert expectations 
(_Thens_). The cumulative result is a chain of actions that execute in sequence 
to model cause and effect. 

When a change to the initial conditions of a deterministic system cannot 
guarantee a predictable outcome, then the behavior of that system can be said 
to be chaotic. Software systems that are generally stable can still exhibit 
chaotic behavior for perhaps only a subset of initial conditions. The chaotic 
behavior takes place on an attractor if a large proportion of these conditions 
converges to and orbits around a chaotic region. The Lorenz attractor, 
discovered by [Edward Lorenz](http:s//en.wikipedia.org/wiki/Edward_Lorenz) 
shows one such orbit that looks like a butterfly. The logo captures this image 
with the proposition that Gwen can detect chaotic behavior.

***

<em>Gwen logo developed by [Nascent Creative](http://www.nascentcreative.com.au)</em>
