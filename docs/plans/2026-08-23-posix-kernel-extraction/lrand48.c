// glibc's srand48/lrand48: the first outputs for a sweep of seeds, including
// seeds above 2^32 (time_t is 64-bit), to pin the algorithm a client must
// reproduce.
#include <stdio.h>
#include <stdlib.h>
int main(void) {
    long seeds[] = {0, 1, 0x330E, 1790000000L, 0xFFFFFFFFL, 0x100000000L, 0x123456789ABL, -1L};
    for (unsigned i = 0; i < sizeof seeds / sizeof *seeds; i++) {
        srand48(seeds[i]);
        printf("LRAND48 seed=%ld:", seeds[i]);
        for (int k = 0; k < 4; k++) printf(" %ld", lrand48());
        printf("\n");
    }
    return 0;
}
