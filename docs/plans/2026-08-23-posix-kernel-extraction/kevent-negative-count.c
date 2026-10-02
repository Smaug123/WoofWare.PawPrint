// kevent(2)'s answer to a wait for a negative number of events: a kqueue with
// nothing registered, with an idle EVFILT_READ registration, and with a ready
// EVFILT_WRITE one, waited on with a NULL timeout for 0, -1, -2 and INT_MIN
// events.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -o /tmp/kn kevent-negative-count.c && /tmp/kn
//
// Measured 2026-10-01 on Darwin 27.0.0 arm64: every one of the twelve rows
// returned 0 at once, errno untouched, even with a registration ready. So a
// negative count is answered as 0 is. (Linux's epoll_wait answers a negative
// maxevents as it answers 0, EINVAL: `epoll-wait.c`, section G.)
#include <errno.h>
#include <limits.h>
#include <stdio.h>
#include <string.h>
#include <sys/event.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
int main(void) {
    alarm(10);
    int counts[] = {0, -1, -2, INT_MIN};
    for (int reg = 0; reg <= 2; reg++) {
        int kq = kqueue();
        int s = socket(AF_INET, SOCK_DGRAM, 0);
        if (reg >= 1) { struct kevent ev; EV_SET(&ev, s, EVFILT_READ, EV_ADD|EV_CLEAR, 0, 0, NULL); kevent(kq, &ev, 1, NULL, 0, NULL); }
        if (reg == 2) { struct kevent ev; EV_SET(&ev, s, EVFILT_WRITE, EV_ADD|EV_CLEAR, 0, 0, NULL); kevent(kq, &ev, 1, NULL, 0, NULL); }
        for (int i = 0; i < 4; i++) {
            struct kevent out[4];
            errno = 0;
            int rv = kevent(kq, NULL, 0, out, counts[i], NULL);
            printf("registrations=%s nevents=%d rv=%d errno=%d(%s)\n", reg==0?"none":reg==1?"idle-read":"ready-write", counts[i], rv, rv<0?errno:0, rv<0?strerror(errno):"-");
        }
        close(s); close(kq);
    }
    return 0;
}
