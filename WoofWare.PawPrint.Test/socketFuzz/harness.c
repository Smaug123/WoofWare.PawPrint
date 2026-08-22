/* The real-kernel side of the socket/epoll differential fuzzer
 * (docs/plans/2026-08-22-socket-epoll-fuzzer.md).
 *
 * Reads op sequences from stdin, one sequence per line, ops separated by
 * spaces (the op language is in the plan doc; SocketFuzz.fs emits it). Each
 * sequence runs in a forked child three times, so its fd table is fresh and
 * instability across the runs is observable. Output, one line per input line:
 *
 *   = <transcript>                    the three runs agreed
 *   ! <t1> | <t2> | <t3>              they did not; exclude from comparison
 *
 * A transcript is one space-separated token per op: "ok", an errno name, or
 * for wait a batch "[data/IN+OUT,...]" in ready-list order with mask bits in
 * canonical IN,OUT,RDHUP,HUP,ERR order.
 *
 * Determinism convention: the emulated kernel signals synchronously at the
 * producing op, so after every op with asynchronous effects (connect, close,
 * accept) the harness sleeps long enough for the loopback edges to land —
 * op order must equal edge order, or the comparison is meaningless.
 *
 * Build and run (see the plan doc):
 *   gcc -O2 -Wall -Wextra -Werror -o harness harness.c && ./harness < seqs.txt
 */
#define _GNU_SOURCE
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

#define MAX_SLOTS 64
#define MAX_EVENTS 64
#define OUT_CAP 8192
/* Loopback handshake and FIN edges land in microseconds; 5ms is a wide
 * margin, and the three-run agreement check catches it being too narrow. */
#define SETTLE_USEC 5000
/* Nothing in the container ever listens on port 1, and bind never assigns
 * privileged ports, so a connect there is a deterministic RST. */
#define DEAD_PORT 1

static int fds[MAX_SLOTS];

static char out[OUT_CAP];
static size_t out_len;

static void emit(const char *token)
{
    size_t n = strlen(token);
    if (out_len + n + 2 > OUT_CAP)
    {
        fprintf(stderr, "transcript overflow\n");
        _exit(1);
    }
    if (out_len > 0)
        out[out_len++] = ' ';
    memcpy(out + out_len, token, n);
    out_len += n;
}

static void emit_errno(void)
{
    const char *name = strerrorname_np(errno);
    if (name != NULL)
        emit(name);
    else
    {
        char buf[32];
        snprintf(buf, sizeof buf, "E%d", errno);
        emit(buf);
    }
}

static void settle(void)
{
    usleep(SETTLE_USEC);
}

static int slot_fd(int slot)
{
    if (slot < 0 || slot >= MAX_SLOTS || fds[slot] < 0)
    {
        fprintf(stderr, "op names slot %d, which holds no fd — the generator is supposed to be constructive\n", slot);
        _exit(1);
    }
    return fds[slot];
}

static void assign_slot(int slot, int fd)
{
    if (slot < 0 || slot >= MAX_SLOTS || fds[slot] >= 0)
    {
        fprintf(stderr, "op assigns slot %d, which is out of range or already taken\n", slot);
        _exit(1);
    }
    fds[slot] = fd;
}

/* SA_* interest bits (the PAL's SocketEvents) to epoll bits, 1:1 as
 * GetEPollEvents maps them; EPOLLET always, as registration always ORs it. */
static uint32_t interest_to_epoll(int mask)
{
    uint32_t ev = EPOLLET;
    if (mask & 0x01)
        ev |= EPOLLIN;
    if (mask & 0x02)
        ev |= EPOLLOUT;
    if (mask & 0x04)
        ev |= EPOLLRDHUP;
    if (mask & 0x08)
        ev |= EPOLLHUP;
    if (mask & 0x10)
        ev |= EPOLLERR;
    return ev;
}

static void mask_string(uint32_t events, char *buf, size_t cap)
{
    buf[0] = '\0';
    const char *parts[5];
    size_t count = 0;
    if (events & EPOLLIN)
        parts[count++] = "IN";
    if (events & EPOLLOUT)
        parts[count++] = "OUT";
    if (events & EPOLLRDHUP)
        parts[count++] = "RDHUP";
    if (events & EPOLLHUP)
        parts[count++] = "HUP";
    if (events & EPOLLERR)
        parts[count++] = "ERR";
    uint32_t known = EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLHUP | EPOLLERR;
    if (events & ~known)
    {
        fprintf(stderr, "epoll_wait reported bits 0x%x outside IN|OUT|RDHUP|HUP|ERR\n", events);
        _exit(1);
    }
    for (size_t i = 0; i < count; i++)
    {
        if (i > 0)
            strncat(buf, "+", cap - strlen(buf) - 1);
        strncat(buf, parts[i], cap - strlen(buf) - 1);
    }
}

static struct sockaddr_in loopback(uint16_t port)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return addr;
}

static void run_op(const char *op)
{
    int a, b, c;
    if (sscanf(op, "sock:%d", &a) == 1 && strchr(op + 5, ':') == NULL)
    {
        int fd = socket(AF_INET, SOCK_STREAM | SOCK_NONBLOCK, 0);
        if (fd < 0)
        {
            emit_errno();
            return;
        }
        assign_slot(a, fd);
        emit("ok");
    }
    else if (sscanf(op, "lstn:%d", &a) == 1)
    {
        int fd = slot_fd(a);
        struct sockaddr_in addr = loopback(0);
        if (bind(fd, (struct sockaddr *)&addr, sizeof addr) < 0 || listen(fd, 8) < 0)
        {
            emit_errno();
            return;
        }
        socklen_t len = sizeof addr;
        if (getsockname(fd, (struct sockaddr *)&addr, &len) < 0)
        {
            emit_errno();
            return;
        }
        emit("ok");
    }
    else if (sscanf(op, "conn:%d:%d", &a, &b) == 2)
    {
        /* The listening endpoint belongs to the socket, not to any one fd of
         * it, so ask the kernel at connect time rather than keeping a
         * slot-keyed table that dup and close would have to maintain. */
        struct sockaddr_in addr;
        socklen_t len = sizeof addr;
        if (getsockname(slot_fd(b), (struct sockaddr *)&addr, &len) < 0 || addr.sin_port == 0)
        {
            fprintf(stderr, "conn:%d:%d targets a slot whose socket never listened\n", a, b);
            _exit(1);
        }
        int err;
        while ((err = connect(slot_fd(a), (struct sockaddr *)&addr, sizeof addr)) < 0 && errno == EINTR)
            ;
        if (err == 0)
            emit("ok");
        else
            emit_errno();
        settle();
    }
    else if (sscanf(op, "conndead:%d", &a) == 1)
    {
        struct sockaddr_in addr = loopback(DEAD_PORT);
        int err;
        while ((err = connect(slot_fd(a), (struct sockaddr *)&addr, sizeof addr)) < 0 && errno == EINTR)
            ;
        if (err == 0)
            emit("ok");
        else
            emit_errno();
        settle();
    }
    else if (sscanf(op, "acpt:%d:%d", &a, &b) == 2)
    {
        int fd = accept4(slot_fd(a), NULL, NULL, SOCK_NONBLOCK);
        if (fd < 0)
        {
            emit_errno();
            return;
        }
        assign_slot(b, fd);
        emit("ok");
        settle();
    }
    else if (sscanf(op, "close:%d", &a) == 1)
    {
        if (close(slot_fd(a)) < 0)
            emit_errno();
        else
            emit("ok");
        fds[a] = -1;
        settle();
    }
    else if (sscanf(op, "dup:%d:%d", &a, &b) == 2)
    {
        int fd = dup(slot_fd(a));
        if (fd < 0)
        {
            emit_errno();
            return;
        }
        assign_slot(b, fd);
        emit("ok");
    }
    else if (sscanf(op, "port:%d", &a) == 1)
    {
        int fd = epoll_create1(0);
        if (fd < 0)
        {
            emit_errno();
            return;
        }
        assign_slot(a, fd);
        emit("ok");
    }
    else if (sscanf(op, "add:%d:%d:%d", &a, &b, &c) == 3)
    {
        struct epoll_event ev;
        ev.events = interest_to_epoll(c);
        ev.data.u64 = (uint64_t)b;
        if (epoll_ctl(slot_fd(a), EPOLL_CTL_ADD, slot_fd(b), &ev) < 0)
            emit_errno();
        else
            emit("ok");
    }
    else if (sscanf(op, "mod:%d:%d:%d", &a, &b, &c) == 3)
    {
        struct epoll_event ev;
        ev.events = interest_to_epoll(c);
        ev.data.u64 = (uint64_t)b;
        if (epoll_ctl(slot_fd(a), EPOLL_CTL_MOD, slot_fd(b), &ev) < 0)
            emit_errno();
        else
            emit("ok");
    }
    else if (sscanf(op, "del:%d:%d", &a, &b) == 2)
    {
        struct epoll_event ev;
        memset(&ev, 0, sizeof ev);
        if (epoll_ctl(slot_fd(a), EPOLL_CTL_DEL, slot_fd(b), &ev) < 0)
            emit_errno();
        else
            emit("ok");
    }
    else if (sscanf(op, "wait:%d:%d", &a, &b) == 2)
    {
        if (b < 1 || b > MAX_EVENTS)
        {
            fprintf(stderr, "wait maxevents %d out of range\n", b);
            _exit(1);
        }
        struct epoll_event evs[MAX_EVENTS];
        int n = epoll_wait(slot_fd(a), evs, b, 0);
        if (n < 0)
        {
            emit_errno();
            return;
        }
        char batch[OUT_CAP];
        batch[0] = '[';
        batch[1] = '\0';
        for (int i = 0; i < n; i++)
        {
            char mask[64];
            mask_string(evs[i].events, mask, sizeof mask);
            char entry[96];
            snprintf(entry, sizeof entry, "%s%llu/%s", i > 0 ? "," : "", (unsigned long long)evs[i].data.u64, mask);
            strncat(batch, entry, sizeof batch - strlen(batch) - 1);
        }
        strncat(batch, "]", sizeof batch - strlen(batch) - 1);
        emit(batch);
    }
    else
    {
        fprintf(stderr, "unparseable op: %s\n", op);
        _exit(1);
    }
}

/* Run one sequence in a fresh child; the transcript comes back over a pipe.
 * Returns a malloc'd transcript, or NULL if the child failed. */
static char *run_sequence(const char *line)
{
    int pipefd[2];
    if (pipe(pipefd) < 0)
    {
        perror("pipe");
        _exit(1);
    }
    pid_t pid = fork();
    if (pid < 0)
    {
        perror("fork");
        _exit(1);
    }
    if (pid == 0)
    {
        close(pipefd[0]);
        for (int i = 0; i < MAX_SLOTS; i++)
            fds[i] = -1;
        out_len = 0;
        char *copy = strdup(line);
        for (char *op = strtok(copy, " "); op != NULL; op = strtok(NULL, " "))
            run_op(op);
        if (write(pipefd[1], out, out_len) != (ssize_t)out_len)
        {
            perror("write");
            _exit(1);
        }
        /* _exit, never exit: the child shares stdin's file description with
         * the parent, and glibc's exit-time stdio cleanup seeks a seekable
         * stream back by the unread buffered amount — which would make the
         * parent re-read input lines it has already processed. */
        _exit(0);
    }
    close(pipefd[1]);
    char *buf = malloc(OUT_CAP);
    size_t total = 0;
    ssize_t n;
    while ((n = read(pipefd[0], buf + total, OUT_CAP - 1 - total)) > 0)
        total += (size_t)n;
    close(pipefd[0]);
    buf[total] = '\0';
    int status;
    waitpid(pid, &status, 0);
    if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
    {
        free(buf);
        return NULL;
    }
    return buf;
}

int main(void)
{
    signal(SIGPIPE, SIG_IGN);
    char *line = NULL;
    size_t cap = 0;
    ssize_t len;
    while ((len = getline(&line, &cap, stdin)) > 0)
    {
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r'))
            line[--len] = '\0';
        if (len == 0)
            continue;
        char *runs[3];
        for (int i = 0; i < 3; i++)
        {
            runs[i] = run_sequence(line);
            if (runs[i] == NULL)
            {
                /* A child that exits nonzero is a harness/generator bug, not
                 * kernel instability; surface it loudly. */
                fprintf(stderr, "child failed on: %s\n", line);
                _exit(1);
            }
        }
        if (strcmp(runs[0], runs[1]) == 0 && strcmp(runs[1], runs[2]) == 0)
            printf("= %s\n", runs[0]);
        else
            printf("! %s | %s | %s\n", runs[0], runs[1], runs[2]);
        fflush(stdout);
        for (int i = 0; i < 3; i++)
            free(runs[i]);
    }
    free(line);
    return 0;
}
