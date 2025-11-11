#include <stdio.h>
#include "rocc.h"
#include <stdint.h>


void print_binary_uint64(uint64_t value) {
    for (int i = 63; i >= 0; i--) {
        putchar((value & (1ULL << i)) ? '1' : '0');
        if (i % 8 == 0) putchar(' ');  // 8ビットごとに区切り
    }
    putchar('\n');
}

int main(void){

    char str1[64] __attribute__ ((aligned(64))) ="ABCDEFGHelloEFG";
    char str2[64] __attribute__ ((aligned(64))) ="ABCDEFGHelloEFG";


    // unsigned long count = 0; 
    // unsigned long hello = 0;

    unsigned long found = 12; 

    uintptr_t addr = (uintptr_t)&str1;
    printf("Address of c = %p\n", (void*)&str1);
    printf("Binary address: ");
    print_binary_uint64(addr);

    uintptr_t addr1 = (uintptr_t)&str2;
    printf("Address of c = %p\n", (void*)&str2);
    printf("Binary address: ");
    print_binary_uint64(addr1);


    printf("well, Im Starting");

    asm volatile("fence");

    ROCC_INSTRUCTION_DSS(0, found, &str1, &str2, 0);

    printf("%lu",found);

    printf("oh well, hello there");
    printf("why are you doing this to me?");
    
    return 0; 
}



// char string[64] __attribute__ ((aligned (64))) = "The quick brown fox jumped over the lazy dog";

// static inline unsigned long count_chars(char *start, char needle)
// {
// 	unsigned long count;
// 	asm volatile ("fence");
// 	ROCC_INSTRUCTION_DSS(2, count, start, needle, 0);
// 	return count;
// }

// int main(void)
// {
// 	unsigned long count = count_chars(string + 14, 'o');
// 	if (count != 3)
// 		return count + 1;
// 	return 0;
// }