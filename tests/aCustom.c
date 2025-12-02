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

// AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHelloo

volatile char str2[64] __attribute__ ((aligned(64))) ="AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAHelloo";
volatile char str1[64] __attribute__ ((aligned(64))) ="Hello";

// volatile char str2[64] __attribute__ ((aligned(64))) ="XAERFKJANLVLJOALKDJHelloABCDEFGIJKLMNOPQRSTUUTSRQPONMLKJIGFEDCB";
// volatile char str1[64] __attribute__ ((aligned(64))) ="1234567890ABCDEFGHIJKLMNOPQRSYWZVX1234567890QWERTYUIOPASDFGHJKL";

// char string[64] __attribute__ ((aligned (64))) = "The quick brown fox jumped over the lazy dog";


// char str2[64] __attribute__ ((aligned(64))) ="XAERFKJANLVLJOALKDJHelloABCDEFGIJKLMNOPQRSTUUTSRQPONMLKJIGFEDCB";
//63文字+NULL文字まで

// static inline unsigned long find_word(char *search, char *area)
// {
//     unsigned long found = 12; 
// 	asm volatile ("fence");
//     ROCC_INSTRUCTION_DSS(0, found, search, area, 0);
// 	return found;
// }


int main(void){


    // unsigned long count = 0; 
    // unsigned long hello = 0;


    uintptr_t addr1 = (uintptr_t)&str1;
    printf("Address of c = %p\n", (void*)&str1);
    printf("Binary address: ");
    print_binary_uint64(addr1);
    printf("%s\n", str1);



    uintptr_t addr2 = (uintptr_t)&str2;
    printf("Address of c = %p\n", (void*)&str2);
    printf("Binary address: ");
    print_binary_uint64(addr2);
    printf("%s\n", str2);

    // printf("well, Im Starting");


    unsigned long found = 12; 
	asm volatile ("fence");
    ROCC_INSTRUCTION_DSS(0, found, str1, str2, 0);

	// unsigned long found = find_word(str1, str2);

    // printf("%lu",found);

    // printf("oh well, hello there");
    // printf("why are you doing this to me?");
    
    return found; 
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