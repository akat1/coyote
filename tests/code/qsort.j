
qsort : @int x , int n -> bool
with
	int i = 0,
	int j = 0,
	int med = x[0],
	int mniej = 0,
	int wiecej = 0,
	@int m size[n],
	@int w size[n]
is
	if n < 2 then
		^true
	fi.

	for i from 1 to n-1 do
		if med < x[i] then
			w[wiecej] <- x[i].
			wiecej <- wiecej + 1
		else
			m[mniej] <- x[i].
			mniej <- mniej + 1
		fi
	done.

	qsort(w,wiecej).
	qsort(m,mniej).

	i <- 0.
	
	for j from 0 to mniej-1 do
		x[i] <- m[j].
		i <- i + 1
	done.
	
	x[i] <- med.
	i <- i + 1.

	for j from 0 to wiecej-1 do
		x[i] <- w[j].
		i <- i + 1
	done.

	^true

main :-> int
with
	int i = 0,
	int n = readInt(),
	@int x size [n]
is
	for i from 0 to n-1 do
		x[i] <- readInt()
	done.

	qsort(x,n).

	for i from 0 to n-1 do
		writeInt(x[i]).
		writeChar('\n')
	done.

	^ 0
	
