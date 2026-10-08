// Darwin only: is SO_LINGER_SEC's EDOM screen applied to the seconds given, or
// to their product with 100 as a 32-bit int would hold it? Seconds whose
// product wraps into 0..32767 are the test. Output beside this file.
//
//     cc -Wall -o p linger-sec-range-darwin.c && ./p
#include <errno.h>
#include <stdio.h>
#include <sys/socket.h>
#include <unistd.h>
int main(void) {
    int vs[] = { 42949673, 42949672, 42949999, 42949677, -42949672, 64424509 };
    for (int i = 0; i < 6; i++) {
        int fd = socket(AF_INET, SOCK_STREAM, 0);
        struct linger g = { 1, vs[i] };
        errno = 0; int r = setsockopt(fd, SOL_SOCKET, SO_LINGER_SEC, &g, sizeof g);
        struct linger h = { 77, 77 }; socklen_t l = sizeof h;
        getsockopt(fd, SOL_SOCKET, SO_LINGER, &h, &l);
        printf("SO_LINGER_SEC {1,%d} (x100 wraps to %d) -> %s; SO_LINGER reads {%d,%d}\n", vs[i], (int)((unsigned)vs[i] * 100u), r == 0 ? "OK" : (errno == EDOM ? "EDOM" : "other"), h.l_onoff, h.l_linger);
        close(fd);
    }
    return 0;
}
