pt: int x -> bool
is
	writeInt(x).
	writeChar('\n').
	^true

main: -> int
with
	int n = readInt(),
	@int tab size [n],
	int i = 0,
	int zmian = 0,
	int tmp = 0
is
	for i from 0 to n-1 do
		tab[i] <- readInt()
	done.

	zmian <- 1.

	while zmian > 0 do
		zmian <- 0.
		for i from 0 to n-2 do
			if tab[i] > tab[i+1] then
				tmp <- tab[i].
				tab[i] <- tab[i+1].
				tab[i+1] <- tmp.
				zmian <- zmian+1
			fi
		done
	done.

	for i from 0 to n-1 do
		writeInt(tab[i]).
		writeChar('\n')
	done.

	^0

