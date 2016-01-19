main:  -> int
with
	int i = 0,
	int j = 0,
	int n = readInt(),
	@int tab size [n+1]
is
	for i from 1 to n do 
	    tab[i] <- 1
	done .
	
	for i from 2 to ((n+1) div 2) do
	    if tab[i] == 1 then 
		for j from 2 to (n div i) do
		    tab[i*j] <- 0
		done
	    fi
	done.
	
	
	for i from 2 to n do
	    if tab[i] == 1 then
		writeInt(i).
		writeChar('\n')
	    fi
	done.
	    
	^0
