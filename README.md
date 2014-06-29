About
=====

This program is an interpreter for Post-Turing programs.
It is a solution for the PhD qualification exam in Programming Languages
at Stevens Institute of Technology.
Adriana Compagnoni posed the [exam](https://web.stevens.edu/compsci/graduate/phd/qualsfiles/pl-f07.pdf) in the Fall of 2007.
This program is based on the code given in the appendix of the exam.

Example
=======

You can run the code in DrRacket.
The following code taken from the [exam](https://web.stevens.edu/compsci/graduate/phd/qualsfiles/pl-f07.pdf) shows how to use the interpreter:

	(unparse (run (input-to-tape '(B 1 1 1)) (prog(parse-instrs copyx) 1)))
