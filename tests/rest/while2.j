main: -> int
with
	int x = 0,
	int y = 9
is
	x <- 1.
	while x <= 10 do
		y <- writeInt(x).
		y <- writeChar('\n').
		x <- x + 1
	done.
	^ x

