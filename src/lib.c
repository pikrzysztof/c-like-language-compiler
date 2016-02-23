#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* concat_strings(char* s1, char* s2)
{
	char* a = malloc(strlen(s1) + strlen(s2) + 1);
	strcpy(a, s1);
	strcat(a, s2);
	return a;
}

void printInt(int a)
{
	printf("%i", a);
	fflush(stdout);
}

void printString(char* a)
{
	printf("%s", a);
	fflush(stdout);
}

void error()
{
	printf("runtime error\n");
	fflush(stdout);
	exit(1);
}

int readInt()
{
	int a;
	scanf("%i", &a);
	return a;
}

char* readString()
{
	char* a;
	size_t len;
	getline(&a, &len, stdin);
}

void exxxit()
{
	exit(0);
}

int streq(char* str1, char* str2)
{
	return !strcmp(str1, str2);
}
