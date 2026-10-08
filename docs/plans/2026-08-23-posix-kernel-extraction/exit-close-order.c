// What a process's exit does to the TCP connections it holds, as another
// process watching their peers sees it.
//
// A child connects three sockets to the parent's listener, in the order c1,
// c2, c3, and moves them to descriptors chosen so that descriptor order,
// creation order and their reverses all differ: c1 at 12, c2 at 10, c3 at 11.
// The parent accepts s1, s2, s3 (each the peer of the same-numbered c) and
// watches them in one epoll instance (Linux) or kqueue (Darwin), registered
// in the order s3, s1, s2 so that registration order differs from all four
// too. The child then exits without closing anything, and the parent reads
// back the order in which the three FINs reached it:
//
//   ascending descriptors   s2 s3 s1
//   descending descriptors  s1 s3 s2
//   creation order          s1 s2 s3
//   reverse creation        s3 s2 s1
//   registration order      s3 s1 s2
//
// Section D tells the order the descriptors are closed in from the order the
// connections are released in: c1 is at 10 and also at 22 (a dup), c2 at 11
// and c3 at 12. Releasing each connection as its descriptors are dropped,
// highest first, gives s3 s2 s1; dropping them lowest first and running each
// final release last-in-first-out gives s1 s3 s2; lowest first and in order,
// s2 s3 s1.
//
// Section L adds a fourth connection the child makes and the parent never
// accepts, so it sits in the parent's accept queue, and a listener of the
// child's own (descriptor 4) with a connection from the child's own socket
// (descriptor 5) still unaccepted in it: the parent reports whether the exit
// left its accept queue readable and what accepting from it then gives.
//
// Build: cc -Wall -o exit-close-order exit-close-order.c
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/epoll.h>
#else
#include <sys/event.h>
#endif

static void die(const char *what) {
    perror(what);
    exit(2);
}

static int listener_on_loopback(struct sockaddr_in *bound) {
    int l = socket(AF_INET, SOCK_STREAM, 0);
    if (l < 0) die("socket");
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (bind(l, (struct sockaddr *)&a, sizeof a) < 0) die("bind");
    if (listen(l, 16) < 0) die("listen");
    socklen_t len = sizeof *bound;
    if (getsockname(l, (struct sockaddr *)bound, &len) < 0) die("getsockname");
    return l;
}

static int connect_to(const struct sockaddr_in *to) {
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (c < 0) die("socket");
    if (connect(c, (const struct sockaddr *)to, sizeof *to) < 0) die("connect");
    return c;
}

static void move_to(int from, int to) {
    if (dup2(from, to) < 0) die("dup2");
    close(from);
}

// One trial: the order the FINs of the child's exit reached the parent's
// three accepted sockets, as indices 1..3 into `order`.
static void trial(int with_queue, int aliased, int order[3], int *queued_readable, int *queued_accept) {
    struct sockaddr_in addr;
    int l = listener_on_loopback(&addr);
    int go[2];
    if (pipe(go) < 0) die("pipe");

    pid_t child = fork();
    if (child < 0) die("fork");
    if (child == 0) {
        close(l);
        close(go[1]);
        int c1 = connect_to(&addr);
        int c2 = connect_to(&addr);
        int c3 = connect_to(&addr);
        if (aliased) {
            move_to(c1, 10);
            move_to(c2, 11);
            move_to(c3, 12);
            if (dup2(10, 22) < 0) die("dup2 alias");
        } else {
            move_to(c1, 12);
            move_to(c2, 10);
            move_to(c3, 11);
        }
        if (with_queue) {
            // A connection the parent never accepts.
            move_to(connect_to(&addr), 13);
            // A listener of the child's own, with the child's own connection
            // still unaccepted in it.
            struct sockaddr_in own;
            int ol = listener_on_loopback(&own);
            move_to(ol, 20);
            move_to(connect_to(&own), 21);
        }
        char b;
        if (read(go[0], &b, 1) != 1) die("read go");
        _exit(0);
    }
    close(go[0]);

    int s[3];
    for (int i = 0; i < 3; i++) {
        s[i] = accept(l, NULL, NULL);
        if (s[i] < 0) die("accept");
    }
    if (with_queue) {
        // Let the fourth connection reach the queue.
        usleep(20000);
    }

    int reg[3] = {2, 0, 1};
#ifdef __linux__
    int q = epoll_create1(0);
    if (q < 0) die("epoll_create1");
    for (int k = 0; k < 3; k++) {
        struct epoll_event e;
        memset(&e, 0, sizeof e);
        e.events = EPOLLIN | EPOLLRDHUP | EPOLLET;
        e.data.u64 = (uint64_t)(reg[k] + 1);
        if (epoll_ctl(q, EPOLL_CTL_ADD, s[reg[k]], &e) < 0) die("epoll_ctl");
    }
#else
    int q = kqueue();
    if (q < 0) die("kqueue");
    for (int k = 0; k < 3; k++) {
        struct kevent e;
        EV_SET(&e, s[reg[k]], EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, (void *)(intptr_t)(reg[k] + 1));
        if (kevent(q, &e, 1, NULL, 0, NULL) < 0) die("kevent add");
    }
    // Drain anything registration itself reported.
    struct kevent junk[8];
    struct timespec zero = {0, 0};
    kevent(q, NULL, 0, junk, 8, &zero);
#endif

    if (write(go[1], "x", 1) != 1) die("write go");
    int status;
    if (waitpid(child, &status, 0) < 0) die("waitpid");
    usleep(50000);

#ifdef __linux__
    struct epoll_event got[8];
    int n = epoll_wait(q, got, 8, 1000);
    for (int i = 0; i < 3; i++) order[i] = (i < n) ? (int)got[i].data.u64 : 0;
#else
    struct kevent got[8];
    struct timespec second = {1, 0};
    int n = kevent(q, NULL, 0, got, 8, &second);
    for (int i = 0; i < 3; i++) order[i] = (i < n) ? (int)(intptr_t)got[i].udata : 0;
#endif
    if (n != 3) {
        fprintf(stderr, "expected 3 events, got %d\n", n);
    }

    *queued_readable = -1;
    *queued_accept = 0;
    if (with_queue) {
        // Is the listener still readable, and what does accepting give?
        struct sockaddr_in peer;
        socklen_t len = sizeof peer;
        int flags_ok = 1;
#ifdef __linux__
        struct epoll_event e;
        memset(&e, 0, sizeof e);
        e.events = EPOLLIN;
        int lq = epoll_create1(0);
        epoll_ctl(lq, EPOLL_CTL_ADD, l, &e);
        struct epoll_event r;
        *queued_readable = epoll_wait(lq, &r, 1, 0);
        close(lq);
#else
        struct kevent e;
        int lq = kqueue();
        EV_SET(&e, l, EVFILT_READ, EV_ADD, 0, 0, NULL);
        struct kevent r;
        struct timespec zero2 = {0, 0};
        *queued_readable = kevent(lq, &e, 1, &r, 1, &zero2);
        close(lq);
#endif
        (void)flags_ok;
        int a = accept(l, (struct sockaddr *)&peer, &len);
        if (a < 0) {
            *queued_accept = -errno;
        } else {
            char b;
            ssize_t got_bytes = read(a, &b, 1);
            *queued_accept = (got_bytes == 0) ? 1 : (got_bytes < 0 ? -1000 - errno : 2);
            close(a);
        }
    }

    for (int i = 0; i < 3; i++) close(s[i]);
    close(q);
    close(l);
    close(go[1]);
}

int main(void) {
    for (int section = 0; section <= 2; section++) {
        int with_queue = section == 1;
        int aliased = section == 2;
        printf("section %s\n", aliased ? "D" : with_queue ? "L" : "O");
        for (int t = 0; t < 20; t++) {
            int order[3], readable, accepted;
            trial(with_queue, aliased, order, &readable, &accepted);
            printf("  trial %2d: s%d s%d s%d", t, order[0], order[1], order[2]);
            if (with_queue) {
                printf("  listener readable %d, accept %d", readable, accepted);
            }
            printf("\n");
        }
    }
    return 0;
}
