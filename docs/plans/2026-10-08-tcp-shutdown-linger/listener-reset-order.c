// Order in which a closing listener's queued clients' resets reach one event
// queue: q0..q3 connect in that order (q0 oldest), each registered
// edge-triggered (epoll EPOLLET IN|OUT|RDHUP; kqueue EVFILT_READ EV_CLEAR) in
// a registration order that differs from the connect order, the queue is
// drained, the listener closes (with SO_LINGER off, or set to {1, 0} just
// before the close), and the queue is read. Each line names the order the
// clients were registered in, and lists the events in the order the queue
// delivered them: epoll's events mask, or kqueue's EV_EOF and fflags.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/listener-reset-order listener-reset-order.c && /tmp/listener-reset-order
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/listener-reset-order.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root,
// default sysctls) and Darwin 27.0.0 arm64 (uid 501), twice on each, with the
// same output both times: listener-reset-order.linux-6.18.5-aarch64.txt and
// listener-reset-order.darwin-27.0.txt. On both, the resets arrive oldest
// connection first, whatever the registration order, as Linux's
// inet_csk_listen_stop and XNU's soclose walk the accept queue from its head.
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static void settle(void) { usleep(30000); }

int main(void)
{
    int orders[3][4] = { { 0, 1, 2, 3 }, { 3, 2, 1, 0 }, { 2, 0, 3, 1 } };
    for (int o = 0; o < 3; o++) {
        for (int linger = 0; linger < 2; linger++) {
            int l = socket(AF_INET, SOCK_STREAM, 0);
            struct sockaddr_in a = { .sin_family = AF_INET, .sin_addr.s_addr = htonl(INADDR_LOOPBACK) };
#ifndef __linux__
            a.sin_len = sizeof a;
#endif
            bind(l, (struct sockaddr *)&a, sizeof a);
            listen(l, 8);
            socklen_t al = sizeof a;
            getsockname(l, (struct sockaddr *)&a, &al);
            int q[4];
            for (int k = 0; k < 4; k++) {
                q[k] = socket(AF_INET, SOCK_STREAM, 0);
                if (connect(q[k], (struct sockaddr *)&a, sizeof a) < 0) { perror("connect"); exit(1); }
                settle();
            }
#ifdef __linux__
            int ep = epoll_create1(0);
            for (int k = 0; k < 4; k++) {
                int i = orders[o][k];
                struct epoll_event ev = { .events = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET, .data.u32 = i };
                epoll_ctl(ep, EPOLL_CTL_ADD, q[i], &ev);
            }
            struct epoll_event out[8];
            epoll_wait(ep, out, 8, 0);
#else
            int kq = kqueue();
            for (int k = 0; k < 4; k++) {
                int i = orders[o][k];
                struct kevent ch;
                EV_SET(&ch, q[i], EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, (void *)(long)i);
                kevent(kq, &ch, 1, NULL, 0, NULL);
            }
            struct kevent out[8];
            struct timespec zero = { 0, 0 };
            kevent(kq, NULL, 0, out, 8, &zero);
#endif
            if (linger) {
                struct linger lg = { 1, 0 };
                setsockopt(l, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
            }
            close(l);
            settle();
            printf("registered %d%d%d%d linger %d: ", orders[o][0], orders[o][1], orders[o][2], orders[o][3], linger);
#ifdef __linux__
            int n = epoll_wait(ep, out, 8, 0);
            for (int k = 0; k < n; k++) printf("q%u(0x%x) ", out[k].data.u32, out[k].events);
#else
            int n = kevent(kq, NULL, 0, out, 8, &zero);
            for (int k = 0; k < n; k++) printf("q%ld(%s/%u) ", (long)out[k].udata, (out[k].flags & EV_EOF) ? "EOF" : "", out[k].fflags);
#endif
            printf("\n");
            for (int k = 0; k < 4; k++) close(q[k]);
        }
    }
    return 0;
}
