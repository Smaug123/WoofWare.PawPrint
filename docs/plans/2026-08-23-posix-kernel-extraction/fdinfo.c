// Does the interest mask a caller passes to epoll_ctl survive verbatim into
// anything userspace can read back? /proc/<pid>/fdinfo/<epfd> is the only such
// surface. If EPOLLERR|EPOLLHUP are forced on by the kernel, then a
// registration that asked for them is indistinguishable from one that did not,
// which is what WoofWare.PosixKernel's three-bit interest record assumes.
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/epoll.h>
#include <sys/socket.h>

static void show(const char *label, int epfd)
{
    char path[64];
    snprintf(path, sizeof path, "/proc/self/fdinfo/%d", epfd);
    FILE *f = fopen(path, "r");
    if (!f) { perror("fopen"); exit(1); }
    char line[256];
    printf("%-28s |", label);
    while (fgets(line, sizeof line, f)) {
        if (strncmp(line, "tfd:", 4) == 0) {
            line[strcspn(line, "\n")] = 0;
            printf(" %s", line);
        }
    }
    printf("\n");
    fclose(f);
}

static void row(const char *label, uint32_t events)
{
    int sv[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, sv) < 0) { perror("socketpair"); exit(1); }
    int epfd = epoll_create1(0);
    if (epfd < 0) { perror("epoll_create1"); exit(1); }
    struct epoll_event evt;
    memset(&evt, 0, sizeof evt);
    evt.events = events;
    evt.data.u64 = 0x1234;
    if (epoll_ctl(epfd, EPOLL_CTL_ADD, sv[0], &evt) < 0) { perror("epoll_ctl"); exit(1); }
    show(label, epfd);
    close(epfd);
    close(sv[0]);
    close(sv[1]);
}

int main(void)
{
    printf("EPOLLIN=0x%x EPOLLOUT=0x%x EPOLLRDHUP=0x%x EPOLLERR=0x%x EPOLLHUP=0x%x EPOLLET=0x%x\n",
           EPOLLIN, EPOLLOUT, EPOLLRDHUP, EPOLLERR, EPOLLHUP, (unsigned) EPOLLET);

    row("interest 0", 0);
    row("EPOLLHUP|EPOLLERR", EPOLLHUP | EPOLLERR);
    row("EPOLLIN", EPOLLIN);
    row("EPOLLIN|EPOLLHUP|EPOLLERR", EPOLLIN | EPOLLHUP | EPOLLERR);
    row("EPOLLIN|EPOLLET", EPOLLIN | EPOLLET);
    row("EPOLLIN|ET|HUP|ERR", EPOLLIN | EPOLLET | EPOLLHUP | EPOLLERR);

    // And the PAL's own shape: it always ORs EPOLLET, and maps SA_CLOSE ->
    // EPOLLHUP, SA_ERROR -> EPOLLERR. So these two are the registrations
    // SystemNative_TryChangeSocketEventRegistration makes for SA_READ alone
    // and for SA_READ|SA_CLOSE|SA_ERROR.
    return 0;
}
