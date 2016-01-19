main: -> bool
with
	@int x size [100],
	int n = readInt(),
	int i = 0
is
	i <- n.
	while i > 0 do
		i <- i - 1.
		x[i] <- readInt()
	done.

	i <- -9999999.

	while n > 0 do
		n <- n - 1.
		if x[n] > i then
			i <- x[n]
		fi
	done.

	writeInt(i).
	writeChar('\n').

	^ true

	



