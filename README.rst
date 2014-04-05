CBIC: C Implemented Bare and Ingenuous Compiler
=================================================

Build Requirements
------------------
- flex >= 2.5.37
- bison >= 2.4.3
- gcc >= 4.7.3

Features
---------
- Complex declaration support (`int (*a)[10]`, `int (*f)()`, `int (*g(int ***e[10]))()`, etc.)
- Forward declaration
- Sophisticated error reporting
- User-friendly AST printing
- Small memory footprint
