/*
 * 
 * This software is released under the beerware licence.
 * ( Borrowed from FreeBSD code )
 * 
 * <shm@nation.pl> wrote this file. As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 *this stuff is worth it, you can buy me a beer in return. :)
 *
 *                                                      Mateusz Kocielski
 */



#include <stdio.h>
#include <stdlib.h>

#undef	DEBUG

typedef enum { true = 1, false = 0 } bool;

char
readChar(void)
{
	return getchar();
}

int
readInt(void)
{
	int i;

	scanf("%d",&i);

	return i;
}

bool
writeChar(int c)
{
	return (putchar(c)!=EOF)?true:false;
}

bool
writeInt(int i)
{
	return (printf("%d",i)>0)?true:false;
}

/* arrays */

int	*
buildArrayRec(int m, int n, int *tab)
{
	int i;
	int *nArray;

	if ( m == n )
		return NULL;
	else
	{
		nArray = (int *)calloc(sizeof(int),tab[m]);

		for ( i = 0 ; i < tab[m] ; i++ )
		{
			nArray[i] = (int)buildArrayRec(m+1,n,tab);
#ifdef DEBUG
			printf("%x\n",nArray[i]);
#endif
		}

		return nArray;
	}
}

int	*
buildArray(int n, ...)
{
	int *tab = (&n);

	tab++;

	return buildArrayRec(0,n,tab);

}

int
arrayGetValue(int *array, int n, ...)
{
	int *tab = (int *)&n;
	int i;

	tab++;
#ifdef DEBUG
	printf("%x\n",tab[0]);
#endif

	for ( i = 0 ; i < n-1 ; i++ )
	{
#ifdef DEBUG
		printf("%p\n",array);
#endif
		array = (int *)array[tab[i]];
#ifdef DEBUG
		printf("%p\n",array);
#endif
	}

	return array[tab[n-1]];
}

void
arraySetValue(int val, int *array, int n, ...)
{
	int *tab = (int *)&n;
	int i;

	tab++;

	for ( i = 0 ; i < n-1 ; i++ )
		array = (int *)array[tab[i]];

	array[tab[n-1]] = val;

	return;
}

void
freeArray(int m,int *array)
{

	if ( m == 1 )
		return;
	else
	{
		while(*array)
		{
			freeArray(m-1,(int *)*array);
			free((int *)*array);
			array++;
		}
	}
}

