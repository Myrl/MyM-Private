#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdint.h>
int main(int argc, char *argv[])
{
    if (argc == 1)
    {
        fprintf("Usage: assemble <file>");
        exit(1);
    }

    FILE *fsource = fopen(argv[1]);
    fseek(fsource, 0, SEEK_END);
    long long int source_size = ftell(fsource);
    char *source = malloc(source_size);
    fread(source, 1, source_size, fsource);

    int pos = 0;
    while(pos < fsource)
    {
        while(!isalnum(source[pos]))
            ++pos;
        int tlength = 0;
        while(isalpha(source[pos]))
            ++tlength;
       
       
       
