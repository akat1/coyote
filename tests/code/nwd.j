main:  -> int
with
	int x = 0,
	int y = 0
is
	x <- readInt().
	y <- readInt().
	writeInt(nwd(x,y)).
	writeChar('\n').
	^0

nwd: int a, int b -> int
is
	while (a != b)  and (a > 0) and (b > 0) do
		if a > b then 
			a <- a - b 
		else 
			b <- b - a
		fi
	done.
	^ a

