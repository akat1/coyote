main: -> int
with
	int x = 0
is
	for x from 0 to 10 do
		writeInt(x).
		writeChar('\n')
	done.
	^x

