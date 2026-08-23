// Does a *positive* timeout still return immediately when the only thing
// making the entry ready is an output-only condition -- an unrequested HUP, or
// NVAL?  `pollmulti.c` measured immediate-return-at-any-timeout only for a
// *requested* POLLOUT, and PawPrint's ready predicate asserts these two cases,
// so measure them rather than inferring from poll semantics.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <poll.h>
#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>

static double now_ms(void) {
    struct timespec t; clock_gettime(CLOCK_MONOTONIC, &t);
    return t.tv_sec * 1000.0 + t.tv_nsec / 1e6;
}
static void show(short r) {
    printf("0x%04x", (unsigned short)r);
    if (r & POLLIN)   printf(" IN");
    if (r & POLLPRI)  printf(" PRI");
    if (r & POLLOUT)  printf(" OUT");
    if (r & POLLERR)  printf(" ERR");
    if (r & POLLHUP)  printf(" HUP");
    if (r & POLLNVAL) printf(" NVAL");
}
static void timed(int fd, short events, int timeout, const char *label) {
    struct pollfd p; p.fd = fd; p.events = events; p.revents = 0;
    double t0 = now_ms();
    int rv = poll(&p, 1, timeout);
    printf("  %-44s rv=%d elapsed=%7.1fms revents=", label, rv, now_ms() - t0);
    if (rv < 0) printf("errno=%d", errno); else show(p.revents);
    printf("\n");
}

int main(void) {
    // An idle TCP socket presents OUT|HUP on Linux. Asking for POLLIN alone,
    // or for nothing at all, leaves HUP as the only reported bit.
    int idle = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    timed(idle, POLLIN, 0,    "1. idle TCP, events=IN,  timeout 0");
    timed(idle, POLLIN, 5000, "2. idle TCP, events=IN,  timeout 5000");
    timed(idle, 0,      5000, "3. idle TCP, events=0,   timeout 5000");
#ifdef __linux__
    // Linux only, and this is the point of the probe rather than an omission:
    // an idle TCP socket presents nothing at all on Darwin, so an infinite
    // wait there blocks forever. The rows above show the same divergence with
    // a bound on it.
    timed(idle, POLLIN, -1,   "4. idle TCP, events=IN,  timeout -1");
#else
    printf("  4. idle TCP, events=IN,  timeout -1        SKIPPED (would block forever: Darwin's idle TCP level is empty)\n");
#endif

    // NVAL is likewise output-only: a never-opened fd must not block.
    timed(4096, POLLIN, 5000, "5. never-opened fd,      timeout 5000");
    timed(4096, 0,      5000, "6. never-opened fd, ev=0,timeout 5000");
    timed(4096, POLLIN, -1,   "7. never-opened fd,      timeout -1");

    // Undefined request bits must be ignored rather than rejected.
    struct pollfd p; p.fd = idle; p.events = (short)0x7FC0; p.revents = 0;
    errno = 0;
    int rv = poll(&p, 1, 0);
    printf("  8. idle TCP, events=0x7FC0, timeout 0  rv=%d errno=%d revents=", rv, errno);
    show(p.revents); printf("\n");
    return 0;
}
