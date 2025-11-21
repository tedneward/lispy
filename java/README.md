# Java lispy
A port of the Norvig lis.py over to Java. Mostly so that I have a tiny Lisp to play around with as a seed for future experiments.

Python is an infinitely easier language by which to build a Lisp. It's intrinsically dynamically-typed, just as Lisp is, but also because Python has adopted the "everything is an object" mentality far deeper than Java ever did--an object represents an executable block of code if it has an "__apply__()" method, it can be converted to a string via a "__str__()" method, it is an iterable collection if it has an "__iter__()" method, and so on. These "intrinsic" methods' presence or absence means that any object could potentially be executable, string-convertible, or iterable.

Doing this in Java means either embracing a very dynamically-typed approach to Java (which was my first inclination, for example have the SExprReader return `Object`), or accepting the need for some greater structure to the code in order to get Java's strong typing into play (which it does now, returning `SExpr`-implementing objects).


