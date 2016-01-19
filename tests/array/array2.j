printTab: @int x -> int
with
	int i = 0
is
	for i from 0 to 99 do
		writeInt(i).
		writeChar(' ').
		writeInt(x[i]).
		writeChar('\n')
	done.
	^0

main: -> int
with
	@int x size [100],
	int i = 0
is
	for i from 0 to 99 do
		x[i] <- 666
	done.

	printTab(x).

	^0

