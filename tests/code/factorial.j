factorial: int n -> int
is

	if n < 2 then 
		^1 
	fi.

	^ n * factorial(n-1)

main: @@char args -> int
is
	writeInt(factorial(readInt())).
	writeChar('\n').
	^0
