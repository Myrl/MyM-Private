#include <inttypes.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

uint64_t memory[65536] = {0};

uint64_t registers[32] = {0};
uint64_t status = 0;
uint8_t acc = 0;

int main(int argc, char *argv[])
{

    if (argc == 1)
    {
        printf("Usage: myass file.o\n");
        exit(1);
    }
    
    FILE *source = fopen(argv[1], "rb");
    if ((source = fopen(argv[1], "rb")) == NULL)
    {
        if (errno == ENOENT)
        {
            printf("%s does not exist.", argv[1]);
        }
        exit(1);
    }
    fseek(source, 0, SEEK_END);
    int program_size = ftell(source)/2;
    fseek(source, 0, SEEK_SET);
    
    uint16_t *program = malloc(program_size*2);
    fread(program, 2, program_size, source);
    
    int imm;
    int reg;
    for (int i = 0; i < program_size; ++i)
    {
        switch(program[i] >> 14)
        {
            case 3:
                switch((program[i])&7)
                {
                    case 0:
                        registers[acc] = ~registers[acc];
                        break;
                    case 1:
                        status ^= (1<<31);
                        break;
                    case 2:
                        printf("%" PRId64 "\n", registers[acc]);
                        break;
                    default:
                        printf("No instruction. 3 prefix.");
                        return 0;
                }
                break;
            case 2:
                reg = program[i]&037;
                switch((program[i]>>6)&7)
                {
                    case 0:
                        acc = reg;
                    break;
                    case 1:
                        if (status & (1<<31))
                            registers[reg] = registers[acc] + registers[reg];
                        else 
                            registers[acc] = registers[acc] + registers[reg];
                        break;
                    case 2:
                        if (status & (1<<31))
                            registers[reg] = registers[acc] - registers[reg];
                        else 
                            registers[acc] = registers[acc] - registers[reg];
                        break;
                    case 3:
                        if (status & (1<<31))
                            registers[reg] = registers[acc] & registers[reg];
                        else 
                            registers[acc] = registers[acc] & registers[reg];
                        break;
                    case 4:
                        if (status & (1<<31))
                            registers[reg] = registers[acc] | registers[reg];
                        else 
                            registers[acc] = registers[acc] | registers[reg];
                        break;
                    case 5:
                        if (status & (1<<31))
                            registers[reg] = registers[acc] ^ registers[reg];
                        else 
                            registers[acc] = registers[acc] ^ registers[reg];
                        break;
                }
                break;
            case 1:
                switch((program[i]>>9) & 3)
                {
                    case 0:
                        imm = program[i]&0777;
                        registers[31] = (imm & 0xff) | ((imm & (1 << 8)) ? (-1 << 8) : 0);
                        break;
                    case 1:
                        printf("Not yet.");

                }
                break;
            case 0:
                printf("Branching not yet.");
                return 0;
                break;
        }
    }
}
