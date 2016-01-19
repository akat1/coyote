merge:@int tab,int a,int b,int c,@int pom->bool
with
int ind=0,
int i=a,
int j=b+1
is
while ( (i<=b) and (j<=c) ) do
      if (tab[i]<tab[j]) then
         pom[ind]<-tab[i].
         ind<-ind+1.
         i<-i+1
      else
         pom[ind]<-tab[j].
         ind<-ind+1.
         j<-j+1 fi
done.

while (i<=b) do
      pom[ind]<-tab[i].
      ind<-ind+1.
      i<-i+1 done.

while (j<=c) do
      pom[ind]<-tab[j].
      ind<-ind+1.
      j<-j+1 done.

for i from a to c do tab[i]<-pom[i-a] done.
^true


merge_sort:@int t,@int pom,int p,int k->bool
is
if (p>=k) then ^true fi.

merge_sort(t,pom,p,(p+k) div 2).
merge_sort(t,pom,(p+k) div 2+1,k).
merge(t,p,(p+k) div 2,k,pom).
^true




main:->bool
with
int s = readInt(),
@int tab size[s],
@int pom size[s],
int n=0,
int i=0
is
n<-s.
for i from 0 to n-1 do tab[i]<-readInt() done.
merge_sort(tab,pom,0,n-1).
for i from 0 to n-1 do writeInt(tab[i]). writeChar('\n') done.
^true


