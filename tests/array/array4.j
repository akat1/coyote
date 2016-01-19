printTab : @int x -> bool
with
	int i = 0
is
	for i from 0 to 9 do
		writeInt(x[i]).
		writeChar('\n')
	done.
	^true

main : -> int
with
	@@int test size [10,10]
is
	test[1,5] <- 10.
	printTab(test[1]).
	^0
	
