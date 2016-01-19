main: -> bool
with
	int x = 9
is
	x <- writeInt(readInt()*readInt()).
	x <- writeChar('\n').
	^ true

