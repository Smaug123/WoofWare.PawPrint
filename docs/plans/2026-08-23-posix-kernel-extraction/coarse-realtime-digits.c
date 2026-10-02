// Whether CLOCK_REALTIME_COARSE's sub-microsecond digits depend on how the
// realtime clock was last set: count readings with such digits over 200 ms,
// then step the clock forward by 1 s + 123 ns with clock_settime (needs
// CAP_SYS_TIME) and count again, then step it back.
#define _GNU_SOURCE
#include <stdio.h>
#include <time.h>
#include <unistd.h>
static long count(void) {
    long n = 0, sub = 0;
    struct timespec s, now, c;
    clock_gettime(CLOCK_MONOTONIC, &s);
    do {
        clock_gettime(CLOCK_REALTIME_COARSE, &c);
        n++;
        if (c.tv_nsec % 1000) sub++;
        clock_gettime(CLOCK_MONOTONIC, &now);
    } while ((now.tv_sec - s.tv_sec) * 1000000000L + now.tv_nsec - s.tv_nsec < 200000000L);
    printf("  readings=%ld with_submicrosecond_digits=%ld\n", n, sub);
    return sub;
}
int main(void) {
    alarm(10);
    printf("before the step:\n");
    count();
    struct timespec t;
    clock_gettime(CLOCK_REALTIME, &t);
    t.tv_sec += 1;
    t.tv_nsec += 123;
    if (t.tv_nsec >= 1000000000) { t.tv_sec++; t.tv_nsec -= 1000000000; }
    printf("clock_settime(+1 s + 123 ns) = %d\n", clock_settime(CLOCK_REALTIME, &t));
    printf("after the step:\n");
    count();
    clock_gettime(CLOCK_REALTIME, &t);
    t.tv_sec -= 1;
    t.tv_nsec -= 123;
    if (t.tv_nsec < 0) { t.tv_sec--; t.tv_nsec += 1000000000; }
    clock_settime(CLOCK_REALTIME, &t);
    return 0;
}
