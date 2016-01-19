main: -> bool
with
	int x = 9
is
	x <- writeInt(readInt() div readInt()).
	x <- writeChar('\n').
	^ true

