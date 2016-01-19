fib : int n -> bool is if n < 2 then ^1 else ^fib(n-2)+fib(n-1) fi
main : -> bool is writeInt(fib(readInt())).^writeChar('\n')
