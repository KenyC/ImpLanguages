Compiler
================================

This is a toy compiler to experiment with designing memory-safe lowish-level language

# IR : intermediate representation

All toy languages are meant to be compiled to IR. IR is a very small language with a mix of C and assembly. In human-readable form, it looks like this:

```c
// sets "to_return" to 1 if initial value of "number" is even

main:
	allocate(number    : int);
	allocate(to_return : int);
	number = 23134;

subBy2:
	number = number - 2; // basic ops supported : +, - ; int constants
	jeq number 0 endEven // 4 forms of conditional jumps support : jeq (:= Jcomp Eq) jneq (:= Jcomp NEq) jmore (:= Jcomp More) jmoreEq (:= Jcomp MoreEq, jump if equal or more)
	jeq number 1 endOdd   
	jump subBy2


endOdd:
	to_return = 1;
	jump end;

endEven:
	to_return = 0;
	jump end;

end:
	free(number);
	free(to_return);
```

In a nutshell:

   1. A program is thought of as a list of scopes with labels
   1. A scope is a series of instructions
   1. A program is run, by executing in a row, the instruction of a particular default scope, labelled "main" here
   1. All data is int
   1. the "Allocate" instruction reserves a piece of memory and associates a given name to that piece of memory. It is a form of "malloc" where the variable is automatically dereferenced.
   1. the "Free" instruction releases the piece of memory associated to the name
   1. the "Set" instruction sets the value of the piece of memory pointed to by a name to the value of expression on the RHS
   1. "Jump" can move from the execution flow of a program from one scope to the beginning of some other (or the same) scope
   1. Expressions are formed from variable names, constants and the operators '+' and '-' 

The IR language can be built from the constructors of the following types (from IR.Syntax): `CScope label 'UnitTy` (instruction, e.g. `jump end`), `CScope label 'IntTy` (expression, e.g. `x+1`), `Module label` (a type synonym for `Map label (CScope label 'UnitTy)`). The Monad `CProgram label _` helps to build modules in Haskell using a similer syntax to the the human-readable form above.  

The IR language can be run on a "Haskell virtual machine" using functions from the module `IR.Backend.Haskell`. This virtual machine meticulously crashes as soon as one of the following memory-unsafe operation happens:

  - Using a name in an expression that is not allocated (~ dangling pointers)
  - Assigning to a name that hasn't been allocated
  - Using names that are allocated but haven't been assigned a value yet
  - Not freeing names at the end of program (~ memory leak)
  - Free'ing something that has never been allocated or has already been freed (~ use after free)
  