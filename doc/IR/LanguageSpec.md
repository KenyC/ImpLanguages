IR language specification
==========================================

## Illustrating all constructs

```c
label1:
	allocate(val: int);
	val = 34;

	allocate(val1: int);
	val1 = val + 32;


	jump label2;


	free(val);

label2:
	jeq val1 val label1;


```

## Meaningful program

Sets "to_return" to 1 if "number" is even

```c

subBy2:
	number = number - 2;
	jeq number 0 endEven
	jeq number 1 endOdd
	jump subBy2


main:
	allocate(number    : int);
	allocate(to_return : int);
	number = 23134;

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
