#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#define wordSize (sizeof(long))

typedef struct string {int length; unsigned char string[1];} tigerString;
tigerString empty = {0, ""};

int main()
{
	extern int _tigermain(long);
	long sl;
	return tigermain(&sl);
}

int *createArray( int size, int init )
{
	int i;
	int *p = malloc((size+1) * wordSize);

	if (NULL == p) {
		printf("Runtime error: sin memoria disponible!\n");
		exit(-1);
	}

	p[0] = size;

	for(i=1; i <= size; i++)
		p[i] = init;

	return p+1;
}

int *checkIndex( int *array, int i )
{
	if (i < 0 || i >= array[-1]) {
		printf("Runtime error: indice fuera de rango!\n");
		exit(-1);
	}

	return array+i;
}

int *createRecord( int size, ... )
{
	va_list va;
	int i;

	int *p = malloc(size * wordSize);

	if (NULL == p) {
		printf("Runtime error: sin memoria disponible!\n");
		exit(-1);
	}

	va_start(va, size);
	for ( i=0; i < size; i++ )
		p[i] = va_arg(va, int);
	va_end(va);

	return p;
}

int *checkNil(int *addr)
{
	if (addr == NULL) {
		printf("Runtime error: referecia nula a record!\n");
		exit(-1);
	}

	return addr;
}

enum Opers {EQ, NEQ, GT, GEQ, LT, LEQ};
int stringComp(int oper, tigerString *s, tigerString *t)
{
	int i, n, res;

	n = s->length <= t->length ? s->length : t->length;

	if (0 == n) {
		if (s->length == t->length) res = 0;
		else if (0 == s->length) res = -1;
		else res = 1;
	}
	else {
		i = 0;
		while (i < n && s->string[i] == t->string[i]) i++;

		if (i == n) {
			if (s->length == t->length) res = 0;
					else if (n == s->length) res = -1;
					else res = 1;
		}
		else res = s->string[i] - t->string[i];
	}

	switch (oper) {
	case EQ:
		return (0 == res);
		break;
	case NEQ:
		return (0 != res);
		break;
	case GT:
		return (0 < res);
		break;
	case GEQ:
		return (0 <= res);
		break;
	case LT:
		return (0 > res);
		break;
	case LEQ:
		return (0 >= res);
		break;
	default:
		printf("Internal error: problemas con runtime.stringComp!\n");
		exit(-1);
		break;
	}

	return 0;
}

void print(tigerString *s)
{
	int i;
	unsigned char *p=s->string;

	for(i=0; i< s->length; i++, p++)
		putchar(*p);
}

void print_int(int i)
{
	printf("%d", i);
}

void flush()
{

	fflush(stdout);
}

tigerString *get_char()
{
	tigerString *res;
	int c = getchar();

	if (EOF == c)
		return &empty;
	else {
		res = malloc(sizeof(tigerString));
		res->length = 1;
		res->string[0] = c;
		return res;
	}
}

int ord(tigerString *s)
{
	if (s->length == 0)
		return -1;
	else
		return s->string[0];
}

tigerString *chr(int i)
{
	tigerString *res;

	if ( i < 0 || i > 255 ) {
		printf("Runtime error: chr(%d) fuera de rango!\n",i);
		exit(-1);
	}

	res = malloc(sizeof(tigerString));
	res->length = 1;
	res->string[0] = i;

	return res;
}

int size(tigerString *s)
{
	return s->length;
}

tigerString *substring(tigerString *s, int first, int n)
{
	int i;
	tigerString *res;

	if (first < 0 || first + n > s->length) {
		printf("Runtime error: substring([%d],%d,%d) fuera de rango!\n", s->length, first, n);
		exit(-1);
	}

	res = malloc(sizeof(tigerString) + n);
	res->length = n;

	for( i=0; i < n; i++)
		res->string[i]= s->string[first+i];

	return res;
}

tigerString *concat(tigerString *a, tigerString *b)
{
	tigerString *res;

	if (a-> length == 0)
		return b;

	else if (b->length == 0)
		return a;

	res = malloc(sizeof(tigerString) + a->length + b->length);
	res->length = a->length + b->length;
	memcpy(res->string, a->string, a->length);
	memcpy(res->string + a->length, b->string, b->length);

	return res;
}

int not(int i)
{
	return !i;
}

void exit_now(int code)
{
	exit(code);
}
