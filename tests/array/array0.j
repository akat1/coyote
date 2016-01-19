main:->bool
with
	@int x size [10]
is
	x[2] <- 666.
	writeInt(x[2]).
	writeChar('\n').
	^true

