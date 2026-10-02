// Linux's coarse clocks, CLOCK_REALTIME_COARSE (5) and CLOCK_MONOTONIC_COARSE
// (6), against their fine counterparts (0 and 1).
//
// 1. clock_getres of every clock id 0..11: the coarse clocks report the tick,
//    which is 1e9 / CONFIG_HZ, a choice made when the kernel is built.
// 2. Over 2 s of back-to-back readings fine0, coarse, fine1 (one id pair at a
//    time): the coarse reading never runs ahead of fine1, never trails fine0
//    by a tick or more, and carries sub-microsecond digits.
// 3. Read together, the two coarse clocks are offset by exactly the realtime
//    clock's offset from the monotonic one: they are one timekeeper reading
//    taken at one tick.
#define _GNU_SOURCE
#include <stdio.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>
static int64_t ns(struct timespec t) { return (int64_t)t.tv_sec * 1000000000 + t.tv_nsec; }
int main(void) {
    alarm(30);
    for (int id = 0; id <= 11; id++) {
        struct timespec r;
        if (clock_getres(id, &r) == 0) printf("GETRES id=%d %lld ns\n", id, (long long)ns(r));
        else printf("GETRES id=%d failed\n", id);
    }
    struct timespec res;
    clock_getres(CLOCK_REALTIME_COARSE, &res);
    int64_t tick = ns(res);
    int pairs[2][2] = {{CLOCK_REALTIME, CLOCK_REALTIME_COARSE}, {CLOCK_MONOTONIC, CLOCK_MONOTONIC_COARSE}};
    for (int p = 0; p < 2; p++) {
        long n = 0, ahead = 0, staleByTick = 0, subMicro = 0;
        int64_t maxLag = 0;
        struct timespec start, now;
        clock_gettime(CLOCK_MONOTONIC, &start);
        do {
            struct timespec f0, c, f1;
            clock_gettime(pairs[p][0], &f0);
            clock_gettime(pairs[p][1], &c);
            clock_gettime(pairs[p][0], &f1);
            n++;
            if (ns(c) > ns(f1)) ahead++;
            if (ns(f0) - ns(c) >= tick) staleByTick++;
            if (ns(f0) - ns(c) > maxLag) maxLag = ns(f0) - ns(c);
            if (c.tv_nsec % 1000 != 0) subMicro++;
            clock_gettime(CLOCK_MONOTONIC, &now);
        } while (ns(now) - ns(start) < 1000000000LL);
        printf("PAIR fine=%d coarse=%d readings=%ld coarse_ahead_of_later_fine=%ld lag_at_least_a_tick=%ld max_lag=%lld ns coarse_with_submicrosecond_digits=%ld\n",
               pairs[p][0], pairs[p][1], n, ahead, staleByTick, (long long)maxLag, subMicro);
    }
    long n = 0, mismatched = 0;
    for (int i = 0; i < 1000000; i++) {
        struct timespec r0, m0, rc, mc, r1, m1;
        clock_gettime(CLOCK_REALTIME, &r0);
        clock_gettime(CLOCK_MONOTONIC, &m0);
        clock_gettime(CLOCK_REALTIME_COARSE, &rc);
        clock_gettime(CLOCK_MONOTONIC_COARSE, &mc);
        clock_gettime(CLOCK_MONOTONIC, &m1);
        clock_gettime(CLOCK_REALTIME, &r1);
        // Skip any sample across which the realtime-monotonic offset moved (NTP
        // slew), or across which a tick may have fallen between the coarse reads.
        int64_t off0 = ns(r0) - ns(m0), off1 = ns(r1) - ns(m1);
        if (off0 / 1000 != off1 / 1000) continue;
        n++;
        int64_t coarseOff = ns(rc) - ns(mc);
        if (coarseOff / 1000 != off0 / 1000) mismatched++;
    }
    printf("OFFSET samples=%ld coarse_offset_differs_from_fine_offset_at_microseconds=%ld\n", n, mismatched);
    return 0;
}
