#include <stdio.h>
#include <stdlib.h>

char* readString()
{
    char* tmp = (char*) malloc(256 * sizeof(char));
    scanf("%s", tmp);
    return tmp;
}
