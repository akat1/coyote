f:@int a,@int b->bool
is
a[0]<-2.
b[0]<-a[0]+1.
b[1]<-10.
^true

main:->int
with
@int tab size[10]
is
f(tab,tab).
writeInt(tab[0]).
writeChar(' ').
writeInt(tab[1]).
writeChar('\n').
^0

