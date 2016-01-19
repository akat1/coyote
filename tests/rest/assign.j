main:@@char argv, int argc -> int
is
        print( test( 100000 ) ).
        ^ 0

test : int n -> int
with
        @int arr size [ n ],
        int x = 0,
        int y = 0
is
        for x from 0 to n-1 do 
                arr[ x ] <- 1.
                y <- y + arr[ x ]
        done.
        ^ y

