// Darwin: bisect the time after which a draw from the process generator goes
// back to getentropy, and whether that time counts from the last reseed or
// from the last draw. Run with DYLD_INSERT_LIBRARIES=libinterpose.dylib (from
// darwin-rng-interpose.c); argument "arc4" draws with arc4random_buf, anything
// else with CCRandomGenerateBytes.
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <CommonCrypto/CommonRandom.h>
static int *calls; static int arc4;
static int draw(void) { unsigned char b[64]; int before = *calls; if (arc4) arc4random_buf(b, 64); else CCRandomGenerateBytes(b, 64); return *calls - before; }
static void force(void) { sleep(8); if (!draw()) printf("(no reseed after 8 s?)\n"); }
int main(int argc, char **argv) {
    alarm(600);
    setvbuf(stdout, NULL, _IOLBF, 0);
    calls = dlsym(RTLD_DEFAULT, "interposed_calls");
    if (!calls) { printf("not interposed\n"); return 1; }
    arc4 = argc > 1 && !strcmp(argv[1], "arc4");
    const char *who = arc4 ? "arc4random_buf" : "CCRandomGenerateBytes";
    int ms[] = {2500, 3000, 3500, 4000, 4500, 4900, 5100, 5500, 6000};
    for (unsigned i = 0; i < sizeof ms / sizeof *ms; i++) {
        force();
        usleep(ms[i] * 1000);
        printf("%s: draw %d ms after a reseed: getentropy calls=%d\n", who, ms[i], draw());
    }
    // Draws every 1 s after a reseed: does each draw restart the clock?
    force();
    for (int s = 1; s <= 8; s++) {
        sleep(1);
        int made = draw();
        printf("%s: draw at %d s after a reseed, with a draw every second: getentropy calls=%d\n", who, s, made);
        if (made) break;
    }
    return 0;
}
