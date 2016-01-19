printArg: @char arg -> bool
with
	int x = 0
is
	x <- 0.

{- while arg[x] != 0 do -}
		writeChar(arg[0]).
		x <- x + 1.
		writeChar(arg[2]).
{-	done. -}

	writeChar('\n').

	^0


main: int argc, @@char argv -> int
is
	writeInt(argc).
	writeChar('\n').
	writeChar(argv[1,0]).
	writeChar('\n').
	printArg(argv[0]).
	^0

