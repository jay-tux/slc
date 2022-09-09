# SLC
*Simple Language Compiler (OCaml)*

## Table of Contents
 - [Table of Contents](#table-of-contents)
 - [SL](#sl)
    - [Grammar](#grammar)
    - [Example 1: Factorial](#example-1-factorial)
    - [Example 2: Syracuse](#example-2-syracuse)
 - [Usage](#usage)
  
## SL
SL is a very, very, very simple language which only supports basic math, 6 variables and basic control flow. All values are parsed as 64-bit integers, and the one print statement can only print those numbers, followed by a newline.

### Grammar
The grammar of the SL is very simple as well; take a look below:
```
variable ::= x | y | z | a | b | c
digit ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
integer ::= <digit> | <digit><integer>
atom ::= <variable> | <integer>
unary_op ::= - | !
binary_op ::= + | - | * | / | < | =
expr ::= <atom> | <unary_op><atom> | <atom><binary_op><atom>
stmt ::= 
    print <variable> | 
    <variable> := <expr> | 
    while <variable> do (<stmt-list>) | 
    if <variable> then (<stmt-list>) else (<stmt-list>)
stmt-list ::= <stmt> | <stmt>;<stmt-list>
program ::= <stmt-list>#
```
Note how the program has to be delimited by a `#` sign. Everything after is regarded as comments. See some examples below.

### Example 1: Factorial
```
x := 5;
a := 1;
c := 0 < x;
while c do (a := a * x ; x := x - 1 ; c := 0 < x);
print a
#
```
This program will compute the factorial of 5 (`5!`) in the while loop. Once the value is computed, it is printed out.  
The variables used are:

 Variable | Usage
 --- | ---
 a | Accumulator for the value
 c | Condition variable
 x | Starting value 

The output for this simple program should be:
```
120
```

### Example 2: Syracuse
```
x := 45 ;
b := x = 1 ;
c := !b ;

while c do
    (   print x ;
        y := x / 2 ;
        z := 2 * y ;
        c := z = x ;
        if c then
            ( x := y )
        else
        (   b := x = 1 ;
            c := !b ;
            x := x * 3 ;
            x := x + 1
        )
    )
#
```
This slightly more complex program will compute every number in the [Syracuse series](https://en.wikipedia.org/wiki/Collatz_conjecture) of 45 and print each of them to the terminal.  
Notice how the modulo-2 computation is done: the value `x` is divided by two and then multiplied again. If it matches with the original value, then the value is even. Otherwise, it's odd.

The variables used are:

 Variable | Usage
 --- | ---
 b | Temporary variable for the conditions
 c | Condition variable
 x | Current value in the Syracuse series
 y | Helper variable for the division/mod-2 check
 z | Contains the result of the division/mod-2 check

The ouptut for this program should be
```
45
136
68
34
17
52
26
13
40
20
10
5
16
8
4
2
1
```

## Usage
*Note: to build SLC, you require the `dune` build system*  
```
slc <infile> [-comp <true/false> -out <outfile>] [-exec <true/false>] [-help] [--help]
```

The `-comp` flag (when used together with `-out`) will compile the code in `infile` to X86-64 assembly (in Intel syntax, no prefixes).  
The `-exec` flag will run the code in `infile`
The `-help` and `--help` flags will both print the help message.

### X86-64 to machine code
Due to a bug (I think), we need another C file to help with the linking. This file should contain the following code:

```c
#include <stdio.h>
#include <stdint.h>

void _printf(int64_t i) {
    printf("%ld\n", i);
}
```
This file is provided in the repository as `helper.c`.

When this file exists, you can just compile the X86-64 output file from SLC and compile it together with the helper file to obtain an executable.