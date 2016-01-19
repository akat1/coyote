main: int x -> int
is
	x <- readInt().
	x <- x + x.
	x <- writeInt(x).
	x <- writeChar('\n').
	^ x

