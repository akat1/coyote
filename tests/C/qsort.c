#include <stdio.h>
#include <stdlib.h>

int cmp(const void *x, const void *y)
{
	int *xx = (int *)x;
	int *yy = (int *)y;

	return (*xx)-(*yy);
}

int main(int argc, char **argv)
{
	int i,n;
	int *tab;


	scanf("%d\n",&n);

	tab = (int *)calloc(sizeof(int),n);

	for ( i = 0 ; i < n ; i++ )
		scanf("%d",&tab[i]);

	qsort(tab,n,sizeof(int),cmp);

	for ( i = 0 ; i < n ; i++ )
		printf("%d\n",tab[i]);


	return 0;
}

