// Which operations on a Darwin pipe activate which kqueue filters.
//
// Every row makes a fresh pipe (both ends non-blocking), registers READ and
// WRITE with EV_CLEAR on both ends in one kqueue (so four registrations: RD:READ,
// RD:WRITE, WR:READ, WR:WRITE), puts the pipe into a starting state, drains the
// kqueue, performs one operation, and prints what the next wait reports. Under
// EV_CLEAR a registration is reported only if something activated it since the
// last wait and its filter is ready now, so the second line is exactly "what the
// operation activated, of what was then ready". A registration that is ready but
// not reported was not activated by the operation.
//
// Each starting state is also reported after its own drain ("before"), so a row
// shows which filters were ready going in.
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -O1 -o /tmp/pa pipe-activation.c && /tmp/pa
//
// Measured 2026-10-02 on Darwin 27.0.0 arm64, twice, identical but for the
// blocking writer's byte counts; pipe-activation.darwin-27.0.txt is one run.
// All four registrations hang on one list per pipe, and every operation that
// touches it evaluates all of them, activating each whose filter is then ready,
// newest-registered first (sys_pipe.c's KNOTE on the pair's read side):
//   - every read, whether it takes bytes, finds the pipe empty (EAGAIN), reads
//     end of file or asks for 0, unless the pipe's buffer is then full (a read
//     of 0 from a full pipe activates nothing, from a 512-byte buffer holding
//     512 too);
//   - every write that leaves bytes in the pipe, including EAGAIN on a full
//     pipe and a write of 0 to a non-empty one, and each pass of a writer
//     asleep in one long write; not a write of 0 to an empty pipe, and not one
//     that fails EPIPE;
//   - the last close of either end; not the close of one of two descriptors
//     onto an end, and not an fcntl.
// Which filters are ready: READ on the read end while bytes are held, WRITE on
// the write end while max(16384, size) - held >= 512, and every filter of both
// ends (READ and WRITE registered on either) with EV_EOF once either end is
// gone.
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/event.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

static int rd, wr, rd2, wr2;

static const char *who(uintptr_t ident)
{
    int fd = (int)ident;
    if (fd == rd) return "RD";
    if (fd == wr) return "WR";
    if (fd == rd2) return "RD2";
    if (fd == wr2) return "WR2";
    return "?";
}

static void drain(int kq, const char *label)
{
    struct kevent out[16];
    struct timespec zero = { 0, 0 };
    int n = kevent(kq, NULL, 0, out, 16, &zero);
    printf("%s\t", label);
    if (n < 0) { printf("kevent: %s\n", strerror(errno)); return; }
    if (n == 0) printf("nothing");
    for (int i = 0; i < n; i++)
        printf("%s%s:%s%s data=%lld", i ? "; " : "", who(out[i].ident), out[i].filter == EVFILT_READ ? "READ" : "WRITE",
               (out[i].flags & EV_EOF) ? "+EOF" : "", (long long)out[i].data);
    printf("\n");
}

static void reg(int kq, int fd)
{
    struct kevent ch[2];
    EV_SET(&ch[0], fd, EVFILT_READ, EV_ADD | EV_CLEAR, 0, 0, NULL);
    EV_SET(&ch[1], fd, EVFILT_WRITE, EV_ADD | EV_CLEAR, 0, 0, NULL);
    if (kevent(kq, ch, 2, NULL, 0, NULL) != 0) printf("#\treg %s: %s\n", who((uintptr_t)fd), strerror(errno));
}

static char block[70000];

static long fill(int fd)
{
    long total = 0;
    for (;;) {
        ssize_t n = write(fd, block, sizeof block);
        if (n <= 0) break;
        total += n;
    }
    for (;;) {
        ssize_t n = write(fd, block, 1);
        if (n <= 0) break;
        total += n;
    }
    return total;
}

enum start { EMPTY_FRESH, HOLDING_1, HOLDING_2, EMPTIED, FULL, WRITER_DUP, READER_DUP, HOLDING_1_WRITER_GONE, HOLDING_512, READER_GONE };
enum op {
    WRITE_1, WRITE_0, WRITE_512, READ_1, READ_ALL, READ_0, READ_512, CLOSE_WRITER, CLOSE_READER, CLOSE_WR_DUP,
    CLOSE_RD_DUP, SET_NONBLOCK_RD, SET_NONBLOCK_WR, READ_EAGAIN_PROBE
};

static const char *start_name(enum start s)
{
    switch (s) {
    case EMPTY_FRESH: return "empty, never written";
    case HOLDING_1: return "holding 1";
    case HOLDING_2: return "holding 2";
    case EMPTIED: return "empty, written and read";
    case FULL: return "full";
    case WRITER_DUP: return "empty, write end dup'd";
    case READER_DUP: return "empty, read end dup'd";
    case HOLDING_1_WRITER_GONE: return "holding 1, writer closed";
    case HOLDING_512: return "holding 512 of a fresh pipe";
    case READER_GONE: return "empty, reader closed";
    }
    return "?";
}

static void row(enum start s, enum op o, const char *op_name)
{
    int p[2];
    pipe(p);
    rd = p[0];
    wr = p[1];
    rd2 = wr2 = -100;
    fcntl(rd, F_SETFL, O_NONBLOCK);
    fcntl(wr, F_SETFL, O_NONBLOCK);
    char buf[70000];
    switch (s) {
    case EMPTY_FRESH: break;
    case HOLDING_1: write(wr, "a", 1); break;
    case HOLDING_2: write(wr, "ab", 2); break;
    case EMPTIED: write(wr, "a", 1); read(rd, buf, 1); break;
    case FULL: printf("#\tfilled with %ld\n", fill(wr)); break;
    case WRITER_DUP: wr2 = dup(wr); break;
    case READER_DUP: rd2 = dup(rd); break;
    case HOLDING_1_WRITER_GONE: write(wr, "a", 1); break;
    case HOLDING_512: write(wr, block, 512); break;
    case READER_GONE: break;
    }
    int kq = kqueue();
    reg(kq, rd);
    reg(kq, wr);
    if (s == HOLDING_1_WRITER_GONE) { close(wr); wr = -100; }
    if (s == READER_GONE) { close(rd); rd = -100; }
    char label[160];
    snprintf(label, sizeof label, "%s\t%s\tbefore", start_name(s), op_name);
    drain(kq, label);
    ssize_t rv = 0;
    int e = 0;
    errno = 0;
    switch (o) {
    case WRITE_1: rv = write(wr, "z", 1); break;
    case WRITE_0: rv = write(wr, "z", 0); break;
    case WRITE_512: rv = write(wr, block, 512); break;
    case READ_1: rv = read(rd, buf, 1); break;
    case READ_ALL: rv = read(rd, buf, sizeof buf); break;
    case READ_0: rv = read(rd, buf, 0); break;
    case READ_512: rv = read(rd, buf, 512); break;
    case CLOSE_WRITER: rv = close(wr); wr = -100; break;
    case CLOSE_READER: rv = close(rd); rd = -100; break;
    case CLOSE_WR_DUP: rv = close(wr2); wr2 = -100; break;
    case CLOSE_RD_DUP: rv = close(rd2); rd2 = -100; break;
    case SET_NONBLOCK_RD: rv = fcntl(rd, F_SETFL, 0); break;
    case SET_NONBLOCK_WR: rv = fcntl(wr, F_SETFL, 0); break;
    case READ_EAGAIN_PROBE: rv = read(rd, buf, 1); break;
    }
    e = rv < 0 ? errno : 0;
    snprintf(label, sizeof label, "%s\t%s\tafter (rv=%zd%s%s)", start_name(s), op_name, rv, e ? " " : "",
             e ? strerror(e) : "");
    drain(kq, label);
    close(kq);
    if (rd >= 0) close(rd);
    if (wr >= 0) close(wr);
    if (rd2 >= 0) close(rd2);
    if (wr2 >= 0) close(wr2);
}

static void *blocking_writer(void *arg)
{
    (void)arg;
    ssize_t n = write(wr, block, 70000);
    printf("#\tblocking write of 70000 returned %zd\n", n);
    return NULL;
}

// A writer asleep in one write of more than the pipe holds, and a reader taking
// bytes: which reads activate what, while the writer resumes behind them.
static void blocking_rows(void)
{
    int p[2];
    pipe(p);
    rd = p[0];
    wr = p[1];
    rd2 = wr2 = -100;
    int kq = kqueue();
    reg(kq, rd);
    reg(kq, wr);
    drain(kq, "blocking writer\tbefore it starts");
    pthread_t t;
    pthread_create(&t, NULL, blocking_writer, NULL);
    usleep(50000);
    drain(kq, "blocking writer\tasleep, having filled the pipe");
    char buf[70000];
    ssize_t n = read(rd, buf, 100);
    usleep(50000);
    printf("#\tread %zd\n", n);
    drain(kq, "blocking writer\tafter a read of 100 (room < 512)");
    n = read(rd, buf, 1000);
    usleep(50000);
    printf("#\tread %zd\n", n);
    drain(kq, "blocking writer\tafter a read of 1000");
    n = read(rd, buf, sizeof buf);
    usleep(50000);
    printf("#\tread %zd\n", n);
    drain(kq, "blocking writer\tafter reading everything");
    pthread_join(t, NULL);
    drain(kq, "blocking writer\tafter it returned");
    close(kq);
    close(rd);
    close(wr);
}

int main(void)
{
    setvbuf(stdout, NULL, _IOLBF, 0);
    alarm(60);
    signal(SIGPIPE, SIG_IGN);
    struct { enum op o; const char *name; } ops[] = {
        { WRITE_1, "write 1" }, { WRITE_0, "write 0" }, { WRITE_512, "write 512" }, { READ_1, "read 1" },
        { READ_ALL, "read all" }, { READ_0, "read 0" }, { READ_512, "read 512" }, { CLOSE_WRITER, "close writer" },
        { CLOSE_READER, "close reader" }, { SET_NONBLOCK_RD, "clear O_NONBLOCK on RD" },
        { SET_NONBLOCK_WR, "clear O_NONBLOCK on WR" },
    };
    enum start starts[] = { EMPTY_FRESH, HOLDING_1, HOLDING_2, EMPTIED, FULL, HOLDING_1_WRITER_GONE };
    for (size_t si = 0; si < sizeof starts / sizeof starts[0]; si++)
        for (size_t oi = 0; oi < sizeof ops / sizeof ops[0]; oi++) {
            if (starts[si] == HOLDING_1_WRITER_GONE &&
                (ops[oi].o == WRITE_1 || ops[oi].o == WRITE_0 || ops[oi].o == WRITE_512 || ops[oi].o == CLOSE_WRITER ||
                 ops[oi].o == SET_NONBLOCK_WR))
                continue;
            row(starts[si], ops[oi].o, ops[oi].name);
        }
    row(HOLDING_512, READ_0, "read 0");
    row(HOLDING_512, WRITE_0, "write 0");
    row(READER_GONE, WRITE_1, "write 1 (EPIPE)");
    row(READER_GONE, WRITE_0, "write 0");
    row(WRITER_DUP, CLOSE_WR_DUP, "close one of two write descriptors");
    row(READER_DUP, CLOSE_RD_DUP, "close one of two read descriptors");
    blocking_rows();
    return 0;
}
