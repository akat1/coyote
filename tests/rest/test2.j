main: -> bool
with
	int x = 9
is
	x <- writeInt(readInt() mod readInt()).
	x <- writeChar('\n').
	^ true

