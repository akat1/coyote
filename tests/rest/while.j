main: -> int
with
	int x = 0,
	int y = 0
is
	while x != 666 do
		x <- readInt().
		y <- writeInt(x).
		y <- writeChar('\n')
	done.
	^x

