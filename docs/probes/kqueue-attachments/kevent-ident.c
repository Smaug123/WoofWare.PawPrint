// What a kqueue registration's identity is, for a descriptor filter and for
// EVFILT_TIMER. Darwin only. Build and run: cc -o kevent-ident kevent-ident.c && ./kevent-ident
//
// T1  EVFILT_TIMER accepts an ident that is no descriptor: above 32 bits, and UINTPTR_MAX.
// T2  The same ident can name a timer and an open descriptor's EVFILT_READ in one kqueue,
//     and closing that descriptor removes the read filter but not the timer.
// T3  A timer added without EV_CLEAR reports with EV_CLEAR set, and data counts expirations.
// T4  A pipe's read end and write end each take EVFILT_READ and EVFILT_WRITE.
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/event.h>
#include <unistd.h>

static int change(int kq, uintptr_t ident, int16_t filter, uint16_t flags, int64_t data) {
    struct kevent kev;
    EV_SET(&kev, ident, filter, flags | EV_RECEIPT, 0, data, NULL);
    struct kevent out;
    int n = kevent(kq, &kev, 1, &out, 1, NULL);
    if (n < 0) return errno;
    if (n == 1 && (out.flags & EV_ERROR)) return (int)out.data;
    return -1000 - n;
}

int main(void) {
    int kq = kqueue();

    printf("T1 timer ident 0x100000001: %d\n", change(kq, (uintptr_t)0x100000001ULL, EVFILT_TIMER, EV_ADD, 1000000));
    printf("T1 timer ident UINTPTR_MAX: %d\n", change(kq, UINTPTR_MAX, EVFILT_TIMER, EV_ADD, 1000000));
    printf("T1 read ident 0x100000000|fd (fd 0): %d\n", change(kq, (uintptr_t)0x100000000ULL, EVFILT_READ, EV_ADD, 0));

    int p[2];
    pipe(p);
    printf("T2 timer ident = pipe read fd %d: %d\n", p[0], change(kq, (uintptr_t)p[0], EVFILT_TIMER, EV_ADD, 1000000));
    printf("T2 read on pipe read fd %d: %d\n", p[0], change(kq, (uintptr_t)p[0], EVFILT_READ, EV_ADD, 0));
    close(p[0]);
    printf("T2 after close: delete read: %d\n", change(kq, (uintptr_t)p[0], EVFILT_READ, EV_DELETE, 0));
    printf("T2 after close: delete timer: %d\n", change(kq, (uintptr_t)p[0], EVFILT_TIMER, EV_DELETE, 0));
    close(p[1]);

    int kq2 = kqueue();
    // 10 ms period, no EV_CLEAR asked for.
    change(kq2, 42, EVFILT_TIMER, EV_ADD, 10);
    usleep(55000);
    struct kevent ev;
    struct timespec zero = {0, 0};
    int n = kevent(kq2, NULL, 0, &ev, 1, &zero);
    printf("T3 n=%d ident=%lu filter=%d flags=0x%x clear=%d data=%lld\n", n, (unsigned long)ev.ident, ev.filter,
           ev.flags, (ev.flags & EV_CLEAR) != 0, (long long)ev.data);
    n = kevent(kq2, NULL, 0, &ev, 1, &zero);
    printf("T3 immediately again: n=%d\n", n);

    int q[2];
    pipe(q);
    int kq3 = kqueue();
    printf("T4 read end EVFILT_READ: %d\n", change(kq3, q[0], EVFILT_READ, EV_ADD, 0));
    printf("T4 read end EVFILT_WRITE: %d\n", change(kq3, q[0], EVFILT_WRITE, EV_ADD, 0));
    printf("T4 write end EVFILT_READ: %d\n", change(kq3, q[1], EVFILT_READ, EV_ADD, 0));
    printf("T4 write end EVFILT_WRITE: %d\n", change(kq3, q[1], EVFILT_WRITE, EV_ADD, 0));
    return 0;
}
