fib:int n->int
with
	int x = 0,
	int y = 0
is
	if n == 0 then
		^1
	fi.
	if n == 1 then
		^1
	fi.
	
	x <- fib(n-2).
	y <- fib(n-1).

	^x+y

main: @@char argv, int argc-> int
with
	int n = 0
is
	n <- writeInt(fib(readInt())).
	n <- writeChar('\n').
	^0

