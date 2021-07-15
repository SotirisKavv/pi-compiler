# PI Compiler

## Description
This is a project for the 'Computation Theory' course that was taught in 2020-21 by M. Lagoudakis.
Its goal was to build a lexical/grammar/syntax analyzer using the Flex and the Bison Tools, in order to interpret a .pi program into a .c one.
This was given in, having worked for it for about 2 days, and got an 8.0/10.0.
The problem was that there were 68 conflicts, yet there was no problem concerning the compilation of the variant programs. Also, there was a problem concering the array assignments, but it was fixed later.
***

## How To Use
In order to compile a .pi program, you have to run the following commands, so as to produce the lexer and the grammar/syntax analyzer:

```
bison -d -v -r all myanalyzer
flex mylexer.l
gcc -o mycomp myanalyzer.tab.c lex.yy.c cgen.c -lfl
```

Now that the compilation program has been created, you can use it to compile/translate/interpret the .pi program into a .c program, using the following commands:

```
./mycomp < [filename].pi > [filename].c
```

At last, the .c file has been generated. But if you open it, you'll see that there are some analysis over the tokens the compiler has found. Delete the lines up until the comment "/* prgram */". After saving these changes, you can compile it using the gcc commands.
***

## Pi-Examples

In this folder, there are some test programs, written in Pi Language, in order to assure that the compilation has been done correctly.

> These files are:
>> * array.pi --> A program to test the array assignments
>> * correct1.pi --> A program to compute the GCD of two Integers 
>> * correct2.pi --> A program to compute the sum of all the natural numbers until a given value, recursively
>> * operators.pi --> A program to test advanced operation (modulo, power, arrays)
>> * primes.pi --> A program to show all the first n prime numbers
>> * qsort.pi --> A program that executes the quicksort algorithm over an array