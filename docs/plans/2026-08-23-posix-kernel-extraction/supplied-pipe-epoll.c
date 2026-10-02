/* What an edge-triggered epoll registration on a pipe's read end sees as the
 * reader reads, when the writer has supplied its bytes and closed (case
 * "closed"), and when the writer is still blocked in one write of more than
 * the pipe holds (case "blocked"). After the ADD and after each read the
 * reader waits SETTLE ms, so that a woken writer has run, then asks
 * epoll_wait with a zero timeout. Linux only.
 *
 * Each line: case size | step read -> ret | events (0 for none; bit 31 set
 * when epoll_wait returned an entry, so an entry with no events shows).
 *
 * Measured 2026-10-01 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14),
 * three runs, identical:
 *
 *   closed 20000:   ADD reports IN|RDNORM|HUP; no read wakes anything.
 *   blocked 200000: ADD reports IN|RDNORM. A read of 100 (no slot freed) and
 *                   one of 4000 (a slot freed and refilled) wake nothing. A
 *                   read emptying the pipe wakes IN|RDNORM (the writer wrote
 *                   into the empty pipe); the next one, after which the
 *                   writer wrote its last bytes into the empty pipe and
 *                   closed, wakes IN|RDNORM|HUP. Nothing after that.
 *   last 65636:     a read of 4096 frees a slot, the writer puts its last 100
 *                   bytes into the pipe that still holds data and closes:
 *                   IN|RDNORM|HUP, so the close wakes the registration.
 *   lastout 65636:  the same, registered for EPOLLOUT only: ADD reports
 *                   nothing, and the close reports HUP. The close's wake is
 *                   not keyed to reading.
 *   blockedout:     registered for EPOLLOUT only: the write into the emptied
 *                   pipe wakes nothing; the close reports HUP. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#define SETTLE_MS 20

static void settle(void) {
    struct timespec t = {0, SETTLE_MS * 1000000L};
    nanosleep(&t, NULL);
}

static unsigned waitEvents(int ep) {
    struct epoll_event ev[4];
    int n = epoll_wait(ep, ev, 4, 0);
    if (n < 0) { perror("epoll_wait"); exit(1); }
    unsigned acc = 0;
    for (int i = 0; i < n; i++) acc |= ev[i].events;
    return n == 0 ? 0 : (acc | 0x80000000u);
}

static char buf[1 << 21];

static void run(const char *name, int blocked, int size, unsigned interest, const int *reads, int nreads) {
    int p[2];
    if (pipe(p) != 0) { perror("pipe"); exit(1); }
    pid_t pid = -1;
    if (blocked) {
        pid = fork();
        if (pid == 0) {
            close(p[0]);
            ssize_t w = write(p[1], buf, size);
            _exit(w == size ? 0 : 3);
        }
    } else {
        if (write(p[1], buf, size) != size) { perror("write"); exit(1); }
    }
    close(p[1]);
    fcntl(p[0], F_SETFL, O_NONBLOCK);
    settle();
    int ep = epoll_create1(0);
    struct epoll_event ev = { .events = interest | EPOLLET, .data.u64 = 1 };
    if (epoll_ctl(ep, EPOLL_CTL_ADD, p[0], &ev) != 0) { perror("ctl"); exit(1); }
    settle();
    printf("%s %d | add -> 0 | 0x%x\n", name, size, waitEvents(ep));
    for (int i = 0; i < nreads; i++) {
        ssize_t r = read(p[0], buf, reads[i]);
        settle();
        printf("%s %d | read %d -> %zd | 0x%x\n", name, size, reads[i], r, waitEvents(ep));
    }
    printf("%s %d | idle -> 0 | 0x%x\n", name, size, waitEvents(ep));
    close(ep);
    close(p[0]);
    if (pid > 0) { int st; waitpid(pid, &st, 0); }
}

int main(void) {
    alarm(60);
    memset(buf, 'x', sizeof buf);
    setvbuf(stdout, NULL, _IOLBF, 0);
    int closedReads[] = { 100, 1000, 4096, 10000, 10000 };
    run("closed", 0, 20000, EPOLLIN | EPOLLRDNORM, closedReads, 5);
    // 100: no slot freed; 4000: frees the first slot (the writer refills it);
    // 65536: everything held; then the rest.
    int blockedReads[] = { 100, 4000, 65536, 65536, 65536, 65536, 65536 };
    run("blocked", 1, 200000, EPOLLIN | EPOLLRDNORM, blockedReads, 7);
    run("blocked", 1, 70000, EPOLLIN | EPOLLRDNORM, blockedReads, 7);
    // The writer's last bytes go into a pipe that is not empty, and it closes:
    // what the close alone wakes, under an interest in reading and under an
    // interest that excludes it.
    int lastReads[] = { 4096, 65536, 65536 };
    run("last", 1, 65636, EPOLLIN | EPOLLRDNORM, lastReads, 3);
    run("lastout", 1, 65636, EPOLLOUT, lastReads, 3);
    run("blockedout", 1, 200000, EPOLLOUT, blockedReads, 7);
    return 0;
}
