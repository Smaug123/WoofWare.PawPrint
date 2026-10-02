// Blocking read(2) and write(2) on a pipe: which of several sleepers a change
// wakes, what a caught signal does to a sleeping transfer (with and without
// part of it done), what the reader leaving does to a sleeping writer, how a
// sleeping write of at most PIPE_BUF bytes resumes, and the corners (a bad
// buffer, O_NONBLOCK set while asleep).
//
// Every pipe is pipe(2), both ends blocking unless stated. "Full" is a pipe
// filled by non-blocking writes: on Linux of 4096 bytes until EAGAIN (sixteen
// full page slots), on Darwin one write of 65536 (which grows the buffer to its
// largest size and fills it). Every signal is SIGUSR1, sent by pthread_kill to
// the sleeping thread, whose handler records that it ran; "restart" means it
// was installed with SA_RESTART. "Asleep" is judged by waiting: a thread that
// has not returned 50-100 ms after it was started or woken is taken to be
// asleep. Elapsed times are CLOCK_MONOTONIC.
//
// Sections:
//   A  readers. A1 three threads each read 1 byte of an empty pipe, started
//      30 ms apart in the order given; then three 1-byte writes 100 ms apart:
//      which thread returns after each, and that no other has. 10 trials of
//      each of the six orders. A2 three readers parked, then one 3-byte
//      write: how many return within 100 ms, 20 trials. A3 two readers that
//      read again as soon as they return, six 1-byte writes 100 ms apart: the
//      order of returns, 20 trials.
//   B  writers. B1 a full pipe; three threads each write 1 byte, started
//      30 ms apart in the order given; then three times, room for one more
//      write (Linux: read 4096 bytes, a slot; Darwin: read 1 byte), 100 ms
//      apart: which thread returns after each. 10 trials of each order. B2
//      the same three parked, room for all three at once (Linux 3 slots,
//      Darwin 3 bytes): how many return within 100 ms, 20 trials.
//   C  a reader asleep on an empty pipe, signalled 50 ms in, with and without
//      restart: rv and errno if it returned within 100 ms; if not, a 3-byte
//      write and the rv then. 5 trials each.
//   D  a writer asleep, signalled 50 ms in, with and without restart, for
//      each prefill (empty, full) and each size in a sweep: rv, errno and the
//      bytes the pipe holds when it returned within 100 ms; if not, the pipe
//      is drained until it does, and rv and the bytes the call put in are
//      recorded. A write that returned before the signal says so.
//   H  (Linux only; needs CAP_SYS_NICE) a sleeper and a hog share CPU 0
//      under SCHED_FIFO, the hog at the higher priority, so that both the
//      sleeper's condition and the signal hold before it next runs. H1 a
//      reader of an empty pipe: a 1-byte write 5 ms in and the signal 10 ms
//      in, or the other way round. H2 a writer of 200000 bytes into an empty
//      pipe (65536 in, asleep): a 4096-byte read 5 ms in and the signal
//      10 ms in, or the other way round. H3 the same with a read of all
//      65536. 20 trials each, no restart: what the call answered.
//   E  a writer asleep, the read end closed by the main thread 50 ms in, for
//      each prefill and size, with SIGPIPE ignored and with a handler that
//      records the thread it ran on: rv, errno, and where SIGPIPE ran.
//   F  how a write of at most PIPE_BUF bytes, asleep on a full pipe, resumes.
//      Linux: a 4096-byte write; read 100 bytes (no slot freed), then 3996
//      (the slot freed). Darwin: a 512-byte write; read 100, 411, then 1; and
//      a 600-byte write for contrast, read 100. After each read, 50 ms later:
//      whether the writer has returned, and the bytes held.
//   G  bad buffers. G1 a blocking read of an empty pipe into (void*)1, a
//      3-byte write 50 ms in: rv, errno, bytes held after. G2 a blocking
//      100-byte write from (void*)1 into a full pipe, 4096 bytes read 50 ms
//      in: rv, errno, bytes held after. G3 a 200000-byte write from (void*)1
//      into an empty pipe.
//   I  O_NONBLOCK set on the description 50 ms into a sleep: I1 a reader of
//      an empty pipe, I2 a writer of 1 byte into a full pipe. Whether it
//      returns within 100 ms; then the reader is given 3 bytes, or the
//      writer room, and its rv recorded.
//   J  the sleeper's condition and a signal both arriving while the whole
//      process is stopped, for a flavour with no SCHED_FIFO to hold the
//      sleeper off the CPU (as `signal-interrupt-requeue.c` section F): a
//      thread sleeps in J-read (an empty pipe), J-write0 (1 byte into a full
//      pipe) or J-partial (200000 bytes into an empty pipe, 65536 of them
//      in); a helper process holding the pipe's other end stops the process,
//      makes the condition hold (writes 1 byte, or reads 4096) and sends
//      SIGUSR1 to the process, in either order, then continues it. Controls
//      make the condition hold alone and send the signal alone. No restart.
//      10 trials each: what the call answered.
//   K  a descriptor closed by the main thread 50 ms into a sleep: a reader of
//      an empty pipe, or a 1-byte writer into a full one, with the close of
//      the descriptor it sleeps through (nothing else onto its description)
//      or of a dup of it. Whether it returns within 100 ms; if not, it is
//      given 3 bytes or 4096 bytes of room, and its rv recorded.
//   L  the timestamps a sleeping transfer leaves: a 1-byte write into a full
//      pipe started ~200 ms in and given room ~1200 ms in, and the write
//      end's mtime and ctime before and after; a read of an empty pipe
//      started ~200 ms in and given 3 bytes ~1200 ms in, and the read end's
//      atime before and after. In ms from the start of each case. L2 what
//      fstat shows ~500 ms into a sleep, and after the call ends some other
//      way: a signal (no SA_RESTART) to a read, to a 1-byte write into a full
//      pipe and to a 200000-byte write into an empty one (65536 in); the read
//      end closing under the two writes; the write end closing under a read.
//      L3 a read and a 1-byte write restarted under SA_RESTART ~1200 ms in,
//      fstat ~1450 ms in, then given data or room ~1700 ms in.
//   N  O_NONBLOCK set on the description while a transfer sleeps. N1 a
//      200000-byte write into a full pipe, N2 into an empty one (65536 in),
//      then 4096 bytes read: rv. N3 (Linux; CAP_SYS_NICE, as H) a reader of
//      an empty pipe or a 4096-byte writer into a full one, the hog giving
//      it bytes or room and taking them back before it runs, 10 trials: what
//      it answers. N4 the same through a stopped process (as J). N5 two
//      sleepers, bytes or room for one, 10 trials: what each answers within
//      100 ms, or that it still sleeps. N6 a write of at most PIPE_BUF bytes
//      asleep on a full pipe, O_NONBLOCK set or not, then a read of 50 bytes,
//      too few to make room for it, 5 trials: whether it returns ("late:"
//      if only once more room was made).
//
// Build and run, from this directory:
//   Darwin: nix develop -c clang -Wall -pthread -o /tmp/pb pipe-blocking.c && /tmp/pb
//   Linux:  container run --rm --cap-add CAP_SYS_NICE -v "$PWD:/probe" gcc:14 sh -c
//             "gcc -Wall -O1 -pthread -o /tmp/pb /probe/pipe-blocking.c && /tmp/pb"
//   An argument names the sections to run, e.g. `/tmp/pb GJ`.
//
// Measured 2026-10-02 on Linux 6.18.5 aarch64 (Apple `container`, gcc:14
// image, glibc 2.41; sections A-I) and Darwin 27.0.0 arm64 (A-G, I, J), once
// in full, G and J again on Darwin, K on both, L on Darwin, and N on both:
//   A  Linux: each 1-byte write woke exactly one reader, the one that parked
//      *first*, in all 60 trials; A3 all 20 returned 010101, so a reader that
//      reads again goes to the back. Darwin: exactly one returned per write,
//      but not by park order (the first-parked first in 6-8 of each 10, any
//      other in the rest; A3 every order): every reader wakes and they race.
//      A2 both: a 3-byte write let all three 1-byte readers return.
//   B  the same for writers. Linux: FIFO in all 60. Darwin: one per room
//      made, the first-parked first in 7-10 of each 10, otherwise any.
//      B2 both: room for three let all three return.
//   C  both: EINTR without restart; with it the read goes on sleeping and
//      returns the 3 bytes written later. The handler ran every time.
//   D  both, every size: a write that had put nothing in answers EINTR
//      without restart and goes on sleeping with it (then completes, the
//      whole count); a write that had put bytes in (65536, of 65537 bytes or
//      more into an empty pipe) returns that count, restart or not, and the
//      handler runs.
//   H  Linux: a reader with a byte to read and a signal pending answered the
//      byte, in all 40 trials, whichever came first. A writer 65536 into
//      200000 answered 69632 when given a slot of room and the signal, and
//      131072 when given room for all, in all 40 each, whichever came first:
//      it fills the room, then returns its count rather than sleeping again.
//   E  Linux: the reader leaving answers EPIPE if the write had put nothing
//      in, and the count it had put in (65536) otherwise; either way SIGPIPE
//      ran on the writing thread. Darwin: EPIPE either way, SIGPIPE on the
//      main thread.
//   F  Linux: a sleeping 4096- or 100-byte write stays asleep while a read
//      frees no slot, and takes a fresh slot when one is freed (held 61540
//      after the 100-byte write, so it did not merge); a 5000-byte write
//      takes one page per freed slot (65536 after the first, 62344 after the
//      second). Darwin: a 512-byte write stays asleep with 100 and 511 bytes
//      free and completes with 512; a 100-byte one with 50 free stays, with
//      100 completes; a 600-byte one takes the 100 bytes each read frees and
//      completes on the last.
//   G  both: G1 the read sleeps, wakes on the data and answers EFAULT, the 3
//      bytes left in the pipe. G2 the write sleeps, wakes on the room and
//      answers EFAULT, taking nothing (held 61440). G3 EFAULT at once.
//   I  both: setting O_NONBLOCK does not wake a sleeping reader or writer,
//      which completes normally when given data or room.
//   J  Darwin: whichever reached the sleeper first decided, in all 10 trials
//      of each: ready then the signal answered the byte (J-read, J-write0)
//      or 69632 (J-partial, the room taken first), the signal then ready
//      answered EINTR or 65536. Controls: ready alone completed (J-partial
//      slept on until its read end was closed, then answered EPIPE), the
//      signal alone EINTR or 65536.
//   N  both: N1 4096 and N2 69632: a write woken by room fills it and
//      returns its count once the flag is set, rather than sleeping again.
//      N3 Linux: every sleeper beaten to what woke it slept on and later
//      took what the unsticking gave it (1 or 4096), never EAGAIN: it
//      re-checks its condition before leaving its sleep. N5 Linux: one
//      sleeper returned and the other slept on, every trial; Darwin: one
//      returned and the other answered EAGAIN, every trial: every sleeper
//      wakes, and one that finds nothing looks at the flag again. N4 Linux:
//      EAGAIN every trial, because the stop itself interrupts the sleep and
//      the call restarts afresh, now non-blocking (a stop, not a wake); on
//      Darwin the stopped sleeper finished its transfer inside the kernel
//      before the helper could take the room back, so N4 cannot make a
//      beaten sleeper there, and N5 is the Darwin measurement. N6: Darwin's
//      512-byte write answered EAGAIN at the 50-byte read when non-blocking
//      (every read wakes every writer), and slept on to complete later when
//      not; Linux's 4096-byte write slept on to complete later either way
//      (no slot was freed, so nothing woke it). 5 trials each.
//   K  Linux (once): neither close woke the sleeper, which completed when
//      given data (3) or room (1): the sleeping call holds its description.
//      Darwin (once): closing a dup changed nothing, but closing the
//      descriptor the call sleeps through woke it at once, the reader with 0
//      (end of file) and the writer with EPIPE.
//   L  Darwin (twice each): the writer's mtime and ctime, and the reader's
//      atime, ended at the moment the call finished (1204-1215 ms), not the
//      moment it went to sleep; L2 nothing moved while the call slept, even
//      for the write that had put 65536 bytes in, and every ending moved them
//      as it happened: EINTR, the count, EPIPE once the reader closed (which
//      a write that answers EPIPE without sleeping does not, pipe-epipe-sweep.c),
//      and end of file; L3 a restart moved them (1203-1208 ms), and the
//      completion moved them again (1707-1718 ms). Not run on Linux, whose
//      pipe timestamps never move (pipe-syscalls.c).
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

#ifdef __linux__
static int64_t now_us(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (int64_t)ts.tv_sec * 1000000 + ts.tv_nsec / 1000;
}
#endif

static void sleep_ms(int ms)
{
    struct timespec ts = { ms / 1000, (long)(ms % 1000) * 1000000L };
    while (nanosleep(&ts, &ts) != 0 && errno == EINTR) { }
}

static atomic_int usr1_ran;

static void on_usr1(int sig)
{
    (void)sig;
    atomic_fetch_add(&usr1_ran, 1);
}

static void install_usr1(int restart)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);
    sa.sa_handler = on_usr1;
    sa.sa_flags = restart ? SA_RESTART : 0;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGUSR1, &sa, NULL);
}

static pthread_t main_thread;
static atomic_int sigpipe_main, sigpipe_other;

static void on_sigpipe(int sig)
{
    (void)sig;
    if (pthread_equal(pthread_self(), main_thread)) atomic_fetch_add(&sigpipe_main, 1);
    else atomic_fetch_add(&sigpipe_other, 1);
}

static int held(int fd)
{
    int n = -1;
    if (ioctl(fd, FIONREAD, &n) != 0) return -1;
    return n;
}

static void set_nonblock(int fd, int on)
{
    int flags = fcntl(fd, F_GETFL);
    fcntl(fd, F_SETFL, on ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK));
}

static unsigned char big[1 << 20];

// Fill `w` as the header says; returns the bytes the pipe then holds.
static int fill(int r, int w)
{
    set_nonblock(w, 1);
#ifdef __linux__
    while (write(w, big, 4096) == 4096) { }
#else
    if (write(w, big, 65536) != 65536) { printf("fill: short\n"); exit(1); }
#endif
    set_nonblock(w, 0);
    return held(r);
}

static void drain_bytes(int r, int n)
{
    unsigned char *buf = malloc((size_t)n);
    int got = 0;
    while (got < n)
    {
        ssize_t k = read(r, buf, (size_t)(n - got));
        if (k <= 0) { printf("drain: %zd errno %d\n", k, errno); exit(1); }
        got += (int)k;
    }
    free(buf);
}

// ---- A ------------------------------------------------------------------

struct reader
{
    int fd;
    int id;
    int count;
    int again;          // read again on return, up to `again` times in all
    atomic_int *order;  // next slot in `log`
    int *log;
    atomic_int returned;
    ssize_t rv;
    int error;
};

static void *reader_main(void *p)
{
    struct reader *r = p;
    int times = r->again > 0 ? r->again : 1;
    for (int i = 0; i < times; i++)
    {
        unsigned char b[8];
        errno = 0;
        r->rv = read(r->fd, b, (size_t)r->count);
        r->error = errno;
        if (r->log) r->log[atomic_fetch_add(r->order, 1)] = r->id;
        atomic_fetch_add(&r->returned, 1);
        if (r->rv <= 0) break;
    }
    return NULL;
}

static const int perms[6][3] = { { 0, 1, 2 }, { 0, 2, 1 }, { 1, 0, 2 }, { 1, 2, 0 }, { 2, 0, 1 }, { 2, 1, 0 } };

static void section_a(void)
{
    for (int p = 0; p < 6; p++)
    {
        int firsts = 0, others = 0;
        char seen[10][8];
        for (int trial = 0; trial < 10; trial++)
        {
            int fds[2];
            pipe(fds);
            atomic_int order = 0;
            int log[8] = { 0 };
            struct reader rs[3];
            pthread_t ts[3];
            for (int i = 0; i < 3; i++)
            {
                memset(&rs[i], 0, sizeof rs[i]);
                rs[i].fd = fds[0];
                rs[i].id = i;
                rs[i].count = 1;
                rs[i].order = &order;
                rs[i].log = log;
            }
            for (int k = 0; k < 3; k++)
            {
                pthread_create(&ts[perms[p][k]], NULL, reader_main, &rs[perms[p][k]]);
                sleep_ms(30);
            }
            sleep_ms(50);
            int stray = 0;
            for (int k = 0; k < 3; k++)
            {
                unsigned char c = (unsigned char)('a' + k);
                write(fds[1], &c, 1);
                sleep_ms(100);
                if (atomic_load(&order) != k + 1) stray = 1;
            }
            for (int i = 0; i < 3; i++) pthread_join(ts[i], NULL);
            for (int k = 0; k < 3; k++) seen[trial][k] = (char)('0' + log[k]);
            seen[trial][3] = 0;
            if (stray) others++;
            if (log[0] == perms[p][0] && log[1] == perms[p][1] && log[2] == perms[p][2]) firsts++;
            close(fds[0]);
            close(fds[1]);
        }
        printf("A1 park order %d%d%d: fifo=%d/10 not-one-per-write=%d returns:", perms[p][0], perms[p][1],
               perms[p][2], firsts, others);
        for (int t = 0; t < 10; t++) printf(" %s", seen[t]);
        printf("\n");
    }

    {
        int tally[4] = { 0 };
        for (int trial = 0; trial < 20; trial++)
        {
            int fds[2];
            pipe(fds);
            atomic_int order = 0;
            int log[8] = { 0 };
            struct reader rs[3];
            pthread_t ts[3];
            for (int i = 0; i < 3; i++)
            {
                memset(&rs[i], 0, sizeof rs[i]);
                rs[i].fd = fds[0];
                rs[i].id = i;
                rs[i].count = 1;
                rs[i].order = &order;
                rs[i].log = log;
                pthread_create(&ts[i], NULL, reader_main, &rs[i]);
                sleep_ms(30);
            }
            sleep_ms(50);
            write(fds[1], "xyz", 3);
            sleep_ms(100);
            tally[atomic_load(&order)]++;
            close(fds[1]);
            for (int i = 0; i < 3; i++) pthread_join(ts[i], NULL);
            close(fds[0]);
        }
        printf("A2 one 3-byte write, three 1-byte readers: returned within 100 ms 0:%d 1:%d 2:%d 3:%d\n", tally[0],
               tally[1], tally[2], tally[3]);
    }

    {
        char seen[20][8];
        for (int trial = 0; trial < 20; trial++)
        {
            int fds[2];
            pipe(fds);
            atomic_int order = 0;
            int log[16] = { 0 };
            struct reader rs[2];
            pthread_t ts[2];
            for (int i = 0; i < 2; i++)
            {
                memset(&rs[i], 0, sizeof rs[i]);
                rs[i].fd = fds[0];
                rs[i].id = i;
                rs[i].count = 1;
                rs[i].again = 3;
                rs[i].order = &order;
                rs[i].log = log;
                pthread_create(&ts[i], NULL, reader_main, &rs[i]);
                sleep_ms(30);
            }
            sleep_ms(50);
            for (int k = 0; k < 6; k++)
            {
                write(fds[1], "q", 1);
                sleep_ms(100);
            }
            for (int i = 0; i < 2; i++) pthread_join(ts[i], NULL);
            for (int k = 0; k < 6; k++) seen[trial][k] = (char)('0' + log[k]);
            seen[trial][6] = 0;
            close(fds[0]);
            close(fds[1]);
        }
        printf("A3 two readers reading again:");
        for (int t = 0; t < 20; t++) printf(" %s", seen[t]);
        printf("\n");
    }
}

// ---- B ------------------------------------------------------------------

struct writer
{
    int fd;
    int id;
    const void *buf;
    size_t count;
    atomic_int *order;
    int *log;
    atomic_int returned;
    ssize_t rv;
    int error;
};

static void *writer_main(void *p)
{
    struct writer *w = p;
    errno = 0;
    w->rv = write(w->fd, w->buf, w->count);
    w->error = errno;
    if (w->log) w->log[atomic_fetch_add(w->order, 1)] = w->id;
    atomic_store(&w->returned, 1);
    return NULL;
}

static void make_room(int r, int writes)
{
#ifdef __linux__
    drain_bytes(r, 4096 * writes);
#else
    drain_bytes(r, writes);
#endif
}

static void section_b(void)
{
    for (int p = 0; p < 6; p++)
    {
        int fifo = 0, stray = 0;
        char seen[10][8];
        for (int trial = 0; trial < 10; trial++)
        {
            int fds[2];
            pipe(fds);
            fill(fds[0], fds[1]);
            atomic_int order = 0;
            int log[8] = { 0 };
            struct writer ws[3];
            pthread_t ts[3];
            unsigned char bytes[3] = { 'a', 'b', 'c' };
            for (int i = 0; i < 3; i++)
            {
                memset(&ws[i], 0, sizeof ws[i]);
                ws[i].fd = fds[1];
                ws[i].id = i;
                ws[i].buf = &bytes[i];
                ws[i].count = 1;
                ws[i].order = &order;
                ws[i].log = log;
            }
            for (int k = 0; k < 3; k++)
            {
                pthread_create(&ts[perms[p][k]], NULL, writer_main, &ws[perms[p][k]]);
                sleep_ms(30);
            }
            sleep_ms(50);
            int bad = atomic_load(&order) != 0;
            for (int k = 0; k < 3; k++)
            {
                make_room(fds[0], 1);
                sleep_ms(100);
                if (atomic_load(&order) != k + 1) bad = 1;
            }
            for (int i = 0; i < 3; i++) pthread_join(ts[i], NULL);
            for (int k = 0; k < 3; k++) seen[trial][k] = (char)('0' + log[k]);
            seen[trial][3] = 0;
            if (bad) stray++;
            if (log[0] == perms[p][0] && log[1] == perms[p][1] && log[2] == perms[p][2]) fifo++;
            close(fds[0]);
            close(fds[1]);
        }
        printf("B1 park order %d%d%d: fifo=%d/10 not-one-per-room=%d returns:", perms[p][0], perms[p][1],
               perms[p][2], fifo, stray);
        for (int t = 0; t < 10; t++) printf(" %s", seen[t]);
        printf("\n");
    }

    {
        int tally[4] = { 0 };
        for (int trial = 0; trial < 20; trial++)
        {
            int fds[2];
            pipe(fds);
            fill(fds[0], fds[1]);
            atomic_int order = 0;
            int log[8] = { 0 };
            struct writer ws[3];
            pthread_t ts[3];
            unsigned char bytes[3] = { 'a', 'b', 'c' };
            for (int i = 0; i < 3; i++)
            {
                memset(&ws[i], 0, sizeof ws[i]);
                ws[i].fd = fds[1];
                ws[i].id = i;
                ws[i].buf = &bytes[i];
                ws[i].count = 1;
                ws[i].order = &order;
                ws[i].log = log;
                pthread_create(&ts[i], NULL, writer_main, &ws[i]);
                sleep_ms(30);
            }
            sleep_ms(50);
            make_room(fds[0], 3);
            sleep_ms(100);
            tally[atomic_load(&order)]++;
            close(fds[0]);
            for (int i = 0; i < 3; i++) pthread_join(ts[i], NULL);
            close(fds[1]);
        }
        printf("B2 room for three at once: returned within 100 ms 0:%d 1:%d 2:%d 3:%d\n", tally[0], tally[1],
               tally[2], tally[3]);
    }
}

// ---- C ------------------------------------------------------------------

static void section_c(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int restart = 0; restart <= 1; restart++)
    {
        install_usr1(restart);
        for (int trial = 0; trial < 5; trial++)
        {
            int fds[2];
            pipe(fds);
            struct reader r;
            memset(&r, 0, sizeof r);
            r.fd = fds[0];
            r.count = 8;
            pthread_t t;
            atomic_store(&usr1_ran, 0);
            pthread_create(&t, NULL, reader_main, &r);
            sleep_ms(50);
            pthread_kill(t, SIGUSR1);
            sleep_ms(100);
            if (atomic_load(&r.returned))
            {
                printf("C %s: returned rv=%zd errno=%d handler=%d\n", restart ? "restart" : "no-restart", r.rv,
                       r.error, atomic_load(&usr1_ran));
                pthread_join(t, NULL);
            }
            else
            {
                write(fds[1], "abc", 3);
                pthread_join(t, NULL);
                printf("C %s: still asleep, then rv=%zd errno=%d handler=%d\n", restart ? "restart" : "no-restart",
                       r.rv, r.error, atomic_load(&usr1_ran));
            }
            close(fds[0]);
            close(fds[1]);
        }
    }
}

// ---- D ------------------------------------------------------------------

static const size_t sizes[] = { 1, 100, 512, 513, 4096, 4097, 8192, 65536, 65537, 100000, 200000, 1 << 20 };
#define NSIZES (sizeof sizes / sizeof sizes[0])

static void section_d(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int restart = 0; restart <= 1; restart++)
    {
        install_usr1(restart);
        for (int prefill = 0; prefill <= 1; prefill++)
        {
            for (size_t s = 0; s < NSIZES; s++)
            {
                int fds[2];
                pipe(fds);
                int before = prefill ? fill(fds[0], fds[1]) : 0;
                struct writer w;
                memset(&w, 0, sizeof w);
                w.fd = fds[1];
                w.buf = big;
                w.count = sizes[s];
                pthread_t t;
                atomic_store(&usr1_ran, 0);
                pthread_create(&t, NULL, writer_main, &w);
                sleep_ms(50);
                const char *mode = restart ? "restart" : "no-restart";
                const char *pre = prefill ? "full" : "empty";
                if (atomic_load(&w.returned))
                {
                    pthread_join(t, NULL);
                    printf("D %s prefill=%s n=%zu: completed before the signal rv=%zd held=%d\n", mode, pre, sizes[s],
                           w.rv, held(fds[0]));
                    close(fds[0]);
                    close(fds[1]);
                    continue;
                }
                int asleep_held = held(fds[0]);
                pthread_kill(t, SIGUSR1);
                sleep_ms(100);
                if (atomic_load(&w.returned))
                {
                    pthread_join(t, NULL);
                    printf("D %s prefill=%s n=%zu: held-asleep=%d returned rv=%zd errno=%d held=%d (put in %d) "
                           "handler=%d\n",
                           mode, pre, sizes[s], asleep_held, w.rv, w.error, held(fds[0]), held(fds[0]) - before,
                           atomic_load(&usr1_ran));
                }
                else
                {
                    // Drain until the writer returns; count what it put in.
                    set_nonblock(fds[0], 1);
                    long drained = 0;
                    unsigned char buf[65536];
                    while (!atomic_load(&w.returned) || held(fds[0]) > 0)
                    {
                        ssize_t k = read(fds[0], buf, sizeof buf);
                        if (k > 0) drained += k;
                        else sleep_ms(1);
                    }
                    pthread_join(t, NULL);
                    printf("D %s prefill=%s n=%zu: held-asleep=%d still asleep, then rv=%zd errno=%d (put in %ld) "
                           "handler=%d\n",
                           mode, pre, sizes[s], asleep_held, w.rv, w.error, drained - before,
                           atomic_load(&usr1_ran));
                }
                close(fds[0]);
                close(fds[1]);
            }
        }
    }
}

// ---- H ------------------------------------------------------------------

#ifdef __linux__
static int pin_fifo(int prio)
{
    cpu_set_t set;
    CPU_ZERO(&set);
    CPU_SET(0, &set);
    if (pthread_setaffinity_np(pthread_self(), sizeof set, &set) != 0) return -1;
    struct sched_param sp = { .sched_priority = prio };
    return pthread_setschedparam(pthread_self(), SCHED_FIFO, &sp);
}

struct h_sleeper
{
    int reading;
    int fd;
    size_t count;
    ssize_t rv;
    int error;
    atomic_int started;
};

static void *h_main(void *p)
{
    struct h_sleeper *h = p;
    int pinned = pin_fifo(10);
    if (pinned != 0)
    {
        h->rv = -100 - pinned;
        atomic_store(&h->started, 1);
        return NULL;
    }
    atomic_store(&h->started, 1);
    errno = 0;
    if (h->reading)
    {
        unsigned char b[16];
        h->rv = read(h->fd, b, h->count);
    }
    else
    {
        h->rv = write(h->fd, big, h->count);
    }
    h->error = errno;
    return NULL;
}

static void section_h(void)
{
    signal(SIGPIPE, SIG_IGN);
    install_usr1(0);
    const char *names[] = { "H1 read", "H2 write, room for a slot", "H3 write, room for all" };
    for (int shape = 0; shape < 3; shape++)
    {
        for (int signal_first = 0; signal_first <= 1; signal_first++)
        {
            char tallies[20][24];
            for (int trial = 0; trial < 20; trial++)
            {
                int fds[2];
                pipe(fds);
                struct h_sleeper h;
                memset(&h, 0, sizeof h);
                h.reading = shape == 0;
                h.fd = h.reading ? fds[0] : fds[1];
                h.count = h.reading ? 16 : 200000;
                pthread_t t;
                pthread_create(&t, NULL, h_main, &h);
                while (!atomic_load(&h.started)) { }
                if (h.rv <= -100)
                {
                    printf("H sleeper could not take SCHED_FIFO on CPU 0: error %d\n", (int)(-100 - h.rv));
                    pthread_join(t, NULL);
                    return;
                }
                sleep_ms(30);
                int hog = pin_fifo(20);
                if (hog != 0)
                {
                    printf("H hog could not take SCHED_FIFO on CPU 0: error %d\n", hog);
                    return;
                }
                int64_t t0 = now_us();
                int made = 0, signalled = 0;
                int ready_at = signal_first ? 10000 : 5000;
                int signal_at = signal_first ? 5000 : 10000;
                while (now_us() - t0 < 40000)
                {
                    int64_t dt = now_us() - t0;
                    if (!made && dt >= ready_at)
                    {
                        if (shape == 0) write(fds[1], "z", 1);
                        else drain_bytes(fds[0], shape == 1 ? 4096 : 65536);
                        made = 1;
                    }
                    if (!signalled && dt >= signal_at)
                    {
                        pthread_kill(t, SIGUSR1);
                        signalled = 1;
                    }
                }
                struct sched_param sp = { .sched_priority = 0 };
                pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
                if (shape != 0)
                {
                    // Unstick a writer the signal did not end.
                    set_nonblock(fds[0], 1);
                    sleep_ms(20);
                }
                close(fds[0]);
                pthread_join(t, NULL);
                if (h.rv < 0) snprintf(tallies[trial], sizeof tallies[trial], "e%d", h.error);
                else snprintf(tallies[trial], sizeof tallies[trial], "%zd", h.rv);
                close(fds[1]);
            }
            printf("%s %s:", names[shape], signal_first ? "signal-first" : "ready-first");
            for (int t = 0; t < 20; t++) printf(" %s", tallies[t]);
            printf("\n");
        }
    }
}
#endif

// ---- E ------------------------------------------------------------------

static void section_e(void)
{
    for (int handled = 0; handled <= 1; handled++)
    {
        if (handled)
        {
            struct sigaction sa;
            memset(&sa, 0, sizeof sa);
            sa.sa_handler = on_sigpipe;
            sigemptyset(&sa.sa_mask);
            sigaction(SIGPIPE, &sa, NULL);
        }
        else
        {
            signal(SIGPIPE, SIG_IGN);
        }
        for (int prefill = 0; prefill <= 1; prefill++)
        {
            for (size_t s = 0; s < NSIZES; s++)
            {
                int fds[2];
                pipe(fds);
                if (prefill) fill(fds[0], fds[1]);
                struct writer w;
                memset(&w, 0, sizeof w);
                w.fd = fds[1];
                w.buf = big;
                w.count = sizes[s];
                atomic_store(&sigpipe_main, 0);
                atomic_store(&sigpipe_other, 0);
                pthread_t t;
                pthread_create(&t, NULL, writer_main, &w);
                sleep_ms(50);
                const char *mode = handled ? "handled" : "ignored";
                const char *pre = prefill ? "full" : "empty";
                if (atomic_load(&w.returned))
                {
                    pthread_join(t, NULL);
                    printf("E %s prefill=%s n=%zu: completed before the close rv=%zd\n", mode, pre, sizes[s], w.rv);
                    close(fds[0]);
                    close(fds[1]);
                    continue;
                }
                close(fds[0]);
                sleep_ms(100);
                int returned = atomic_load(&w.returned);
                if (returned) pthread_join(t, NULL);
                printf("E %s prefill=%s n=%zu: %s rv=%zd errno=%d sigpipe main=%d writer=%d\n", mode, pre, sizes[s],
                       returned ? "returned" : "STILL ASLEEP", w.rv, w.error, atomic_load(&sigpipe_main),
                       atomic_load(&sigpipe_other));
                if (!returned) exit(1);
                close(fds[1]);
            }
        }
    }
    signal(SIGPIPE, SIG_IGN);
}

// ---- F ------------------------------------------------------------------

static void f_case(const char *name, size_t count, const int *reads, int nreads)
{
    int fds[2];
    pipe(fds);
    int before = fill(fds[0], fds[1]);
    struct writer w;
    memset(&w, 0, sizeof w);
    w.fd = fds[1];
    w.buf = big;
    w.count = count;
    pthread_t t;
    pthread_create(&t, NULL, writer_main, &w);
    sleep_ms(50);
    printf("F %s: full=%d asleep=%d", name, before, !atomic_load(&w.returned));
    for (int i = 0; i < nreads; i++)
    {
        drain_bytes(fds[0], reads[i]);
        sleep_ms(50);
        printf(" | read %d: returned=%d held=%d", reads[i], atomic_load(&w.returned), held(fds[0]));
    }
    printf("\n");
    close(fds[0]);
    pthread_join(t, NULL);
    close(fds[1]);
}

static void section_f(void)
{
    signal(SIGPIPE, SIG_IGN);
#ifdef __linux__
    {
        int reads[] = { 100, 3996 };
        f_case("4096-byte write", 4096, reads, 2);
    }
    {
        int reads[] = { 100, 3996 };
        f_case("100-byte write", 100, reads, 2);
    }
    {
        int reads[] = { 100, 3996, 4096 };
        f_case("5000-byte write", 5000, reads, 3);
    }
#else
    {
        int reads[] = { 100, 411, 1 };
        f_case("512-byte write", 512, reads, 3);
    }
    {
        int reads[] = { 50, 50 };
        f_case("100-byte write", 100, reads, 2);
    }
    {
        int reads[] = { 100, 100, 400 };
        f_case("600-byte write", 600, reads, 3);
    }
#endif
}

// ---- G ------------------------------------------------------------------

// A reader whose buffer is (void*)1.
struct g_args { int fd; ssize_t rv; int error; atomic_int returned; };

static void *gread(void *p)
{
    struct g_args *a = p;
    errno = 0;
    a->rv = read(a->fd, (void *)1, 8);
    a->error = errno;
    atomic_store(&a->returned, 1);
    return NULL;
}

static void section_g(void)
{
    signal(SIGPIPE, SIG_IGN);
    {
        int fds[2];
        pipe(fds);
        pthread_t t;
        struct g_args g;
        memset(&g, 0, sizeof g);
        g.fd = fds[0];
        pthread_create(&t, NULL, gread, &g);
        sleep_ms(50);
        int asleep = !atomic_load(&g.returned);
        write(fds[1], "abc", 3);
        sleep_ms(50);
        int returned = atomic_load(&g.returned);
        if (!returned)
        {
            close(fds[1]);
        }
        pthread_join(t, NULL);
        printf("G1 read into (void*)1: asleep=%d returned-on-data=%d rv=%zd errno=%d held=%d\n", asleep, returned,
               g.rv, g.error, held(fds[0]));
        close(fds[0]);
        if (returned) close(fds[1]);
    }
    {
        int fds[2];
        pipe(fds);
        int before = fill(fds[0], fds[1]);
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = (const void *)1;
        w.count = 100;
        pthread_t t;
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(50);
        int asleep = !atomic_load(&w.returned);
        drain_bytes(fds[0], 4096);
        int after_room = held(fds[0]);
        sleep_ms(50);
        int returned = atomic_load(&w.returned);
        if (!returned) close(fds[0]);
        pthread_join(t, NULL);
        printf("G2 100-byte write from (void*)1 into a full pipe (%d): asleep=%d returned-on-room=%d rv=%zd errno=%d "
               "held=%d (after the room was made %d)\n",
               before, asleep, returned, w.rv, w.error, returned ? held(fds[0]) : -1, after_room);
        if (returned) close(fds[0]);
        close(fds[1]);
    }
    {
        int fds[2];
        pipe(fds);
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = (const void *)1;
        w.count = 200000;
        pthread_t t;
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(50);
        int returned = atomic_load(&w.returned);
        if (!returned) close(fds[0]);
        pthread_join(t, NULL);
        printf("G3 200000-byte write from (void*)1 into an empty pipe: returned=%d rv=%zd errno=%d\n", returned, w.rv,
               w.error);
        if (returned) close(fds[0]);
        close(fds[1]);
    }
}

// ---- I ------------------------------------------------------------------

static void section_i(void)
{
    signal(SIGPIPE, SIG_IGN);
    {
        int fds[2];
        pipe(fds);
        struct reader r;
        memset(&r, 0, sizeof r);
        r.fd = fds[0];
        r.count = 8;
        pthread_t t;
        pthread_create(&t, NULL, reader_main, &r);
        sleep_ms(50);
        set_nonblock(fds[0], 1);
        sleep_ms(100);
        int woke = atomic_load(&r.returned);
        if (!woke) write(fds[1], "abc", 3);
        pthread_join(t, NULL);
        printf("I1 reader, O_NONBLOCK set while asleep: returned=%d rv=%zd errno=%d\n", woke, r.rv, r.error);
        close(fds[0]);
        close(fds[1]);
    }
    {
        int fds[2];
        pipe(fds);
        fill(fds[0], fds[1]);
        unsigned char c = 'w';
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = &c;
        w.count = 1;
        pthread_t t;
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(50);
        set_nonblock(fds[1], 1);
        sleep_ms(100);
        int woke = atomic_load(&w.returned);
        if (!woke) make_room(fds[0], 1);
        pthread_join(t, NULL);
        printf("I2 writer, O_NONBLOCK set while asleep: returned=%d rv=%zd errno=%d\n", woke, w.rv, w.error);
        close(fds[0]);
        close(fds[1]);
    }
}

// ---- J ------------------------------------------------------------------

struct j_wait { int call; int fd; ssize_t rv; int error; atomic_int returned; };

static void *j_sleeper(void *p)
{
    struct j_wait *w = p;
    sigset_t usr1;
    sigemptyset(&usr1);
    sigaddset(&usr1, SIGUSR1);
    pthread_sigmask(SIG_UNBLOCK, &usr1, NULL);
    errno = 0;
    unsigned char b[16];
    if (w->call == 0) w->rv = read(w->fd, b, sizeof b);
    else if (w->call == 1) w->rv = write(w->fd, "w", 1);
    else w->rv = write(w->fd, big, 200000);
    w->error = errno;
    atomic_store(&w->returned, 1);
    return NULL;
}

static void section_j(void)
{
    signal(SIGPIPE, SIG_IGN);
    install_usr1(0);
    sigset_t usr1, before;
    sigemptyset(&usr1);
    sigaddset(&usr1, SIGUSR1);
    pthread_sigmask(SIG_BLOCK, &usr1, &before);
    const char *names[] = { "J-read", "J-write0", "J-partial" };
    const char *shapes[] = { "ready-then-signal", "signal-then-ready", "ready-only", "signal-only" };
    for (int call = 0; call < 3; call++)
    {
        for (int shape = 0; shape < 4; shape++)
        {
            char seen[10][16];
            for (int trial = 0; trial < 10; trial++)
            {
                int fds[2], go[2], ready[2];
                if (pipe(fds) != 0 || pipe(go) != 0 || pipe(ready) != 0) { perror("pipe"); exit(1); }
                if (call == 1) fill(fds[0], fds[1]);
                pid_t parent = getpid();
                pid_t helper = fork();
                if (helper == 0)
                {
                    alarm(10);
                    char b = 1;
                    if (write(ready[1], &b, 1) != 1) _exit(3);
                    if (read(go[0], &b, 1) != 1) _exit(4);
                    kill(parent, SIGSTOP);
                    sleep_ms(50);
                    for (int step = 0; step < 2; step++)
                    {
                        int signal_step = shape == 1 ? 0 : 1;
                        if (step == signal_step)
                        {
                            if (shape != 2) kill(parent, SIGUSR1);
                        }
                        else if (shape != 3)
                        {
                            if (call == 0) write(fds[1], "z", 1);
                            else drain_bytes(fds[0], 4096);
                        }
                        sleep_ms(50);
                    }
                    kill(parent, SIGCONT);
                    _exit(0);
                }
                char b;
                if (read(ready[0], &b, 1) != 1) { perror("read"); exit(1); }
                struct j_wait w;
                memset(&w, 0, sizeof w);
                w.call = call;
                w.fd = call == 0 ? fds[0] : fds[1];
                pthread_t t;
                pthread_create(&t, NULL, j_sleeper, &w);
                sleep_ms(50);
                b = 1;
                if (write(go[1], &b, 1) != 1) { perror("write"); exit(1); }
                int status;
                waitpid(helper, &status, 0);
                sleep_ms(100);
                // A sleeper that the controls leave asleep is released here.
                if (!atomic_load(&w.returned))
                {
                    if (call == 0) close(fds[1]);
                    else close(fds[0]);
                }
                pthread_join(t, NULL);
                if (w.rv < 0) snprintf(seen[trial], sizeof seen[trial], "e%d", w.error);
                else snprintf(seen[trial], sizeof seen[trial], "%zd", w.rv);
                close(fds[0]); close(fds[1]);
                close(go[0]); close(go[1]); close(ready[0]); close(ready[1]);
            }
            printf("%s %s:", names[call], shapes[shape]);
            for (int t = 0; t < 10; t++) printf(" %s", seen[t]);
            printf("\n");
        }
    }
    pthread_sigmask(SIG_SETMASK, &before, NULL);
}

// ---- K ------------------------------------------------------------------

static void section_k(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int writing = 0; writing <= 1; writing++)
    {
        for (int dup_kept = 0; dup_kept <= 1; dup_kept++)
        {
            int fds[2];
            pipe(fds);
            if (writing) fill(fds[0], fds[1]);
            int own = writing ? fds[1] : fds[0];
            int other = dup_kept ? dup(own) : -1;
            pthread_t t;
            struct reader r;
            struct writer w;
            unsigned char c = 'k';
            memset(&r, 0, sizeof r);
            memset(&w, 0, sizeof w);
            if (writing)
            {
                w.fd = own;
                w.buf = &c;
                w.count = 1;
                pthread_create(&t, NULL, writer_main, &w);
            }
            else
            {
                r.fd = own;
                r.count = 8;
                pthread_create(&t, NULL, reader_main, &r);
            }
            sleep_ms(50);
            int closed = close(dup_kept ? other : own);
            sleep_ms(100);
            int woke = writing ? atomic_load(&w.returned) : atomic_load(&r.returned);
            if (!woke)
            {
                if (writing) drain_bytes(fds[0], 4096);
                else write(fds[1], "abc", 3);
                sleep_ms(100);
            }
            int returned = writing ? atomic_load(&w.returned) : atomic_load(&r.returned);
            if (!returned)
            {
                printf("K %s, closing %s: close=%d, still asleep after the %s\n", writing ? "writer" : "reader",
                       dup_kept ? "a dup" : "its descriptor", closed, writing ? "room" : "data");
                exit(1);
            }
            pthread_join(t, NULL);
            printf("K %s, closing %s: close=%d woke-on-close=%d rv=%zd errno=%d\n", writing ? "writer" : "reader",
                   dup_kept ? "a dup" : "its descriptor", closed, woke, writing ? w.rv : r.rv,
                   writing ? w.error : r.error);
        }
    }
}

// ---- L ------------------------------------------------------------------

static double ts_ms(struct timespec t, struct timespec base)
{
    return (double)(t.tv_sec - base.tv_sec) * 1000.0 + (double)(t.tv_nsec - base.tv_nsec) / 1e6;
}

#ifdef __APPLE__
#define ATIM st_atimespec
#define MTIM st_mtimespec
#define CTIM st_ctimespec
#else
#define ATIM st_atim
#define MTIM st_mtim
#define CTIM st_ctim
#endif

static void section_l(void)
{
    signal(SIGPIPE, SIG_IGN);
    struct timespec base;
    clock_gettime(CLOCK_REALTIME, &base);
    {
        int fds[2];
        pipe(fds);
        fill(fds[0], fds[1]);
        struct stat before;
        fstat(fds[1], &before);
        unsigned char c = 'l';
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = &c;
        w.count = 1;
        pthread_t t;
        sleep_ms(200);
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(1000);
        drain_bytes(fds[0], 4096);
        pthread_join(t, NULL);
        struct stat after;
        fstat(fds[1], &after);
        printf("L writer (asleep at ~200 ms, room at ~1200 ms): rv=%zd mtime %.0f -> %.0f ms, ctime %.0f -> %.0f ms\n",
               w.rv, ts_ms(before.MTIM, base), ts_ms(after.MTIM, base), ts_ms(before.CTIM, base),
               ts_ms(after.CTIM, base));
        close(fds[0]);
        close(fds[1]);
    }
    clock_gettime(CLOCK_REALTIME, &base);
    {
        int fds[2];
        pipe(fds);
        struct stat before;
        fstat(fds[0], &before);
        struct reader r;
        memset(&r, 0, sizeof r);
        r.fd = fds[0];
        r.count = 8;
        pthread_t t;
        sleep_ms(200);
        pthread_create(&t, NULL, reader_main, &r);
        sleep_ms(1000);
        write(fds[1], "abc", 3);
        pthread_join(t, NULL);
        struct stat after;
        fstat(fds[0], &after);
        printf("L reader (asleep at ~200 ms, data at ~1200 ms): rv=%zd atime %.0f -> %.0f ms\n", r.rv,
               ts_ms(before.ATIM, base), ts_ms(after.ATIM, base));
        close(fds[0]);
        close(fds[1]);
    }
    // L2: what fstat shows while the call sleeps, and after it ends some
    // other way than by data or room: a signal (no SA_RESTART), or the other
    // end closing. 0 a read signalled, 1 a 1-byte write into a full pipe
    // signalled, 2 a 200000-byte write into an empty pipe (65536 in)
    // signalled, 3 the same with the read end closed, 4 a read with the
    // write end closed.
    install_usr1(0);
    const char *shapes[] = { "read, signalled", "write of 1 into full, signalled", "write of 200000, signalled",
                             "write of 200000, reader closes", "read, writer closes", "write of 1 into full, reader closes" };
    for (int shape = 0; shape < 6; shape++)
    {
        clock_gettime(CLOCK_REALTIME, &base);
        int fds[2];
        pipe(fds);
        int writing = (shape >= 1 && shape <= 3) || shape == 5;
        if (shape == 1 || shape == 5) fill(fds[0], fds[1]);
        int fd = writing ? fds[1] : fds[0];
        struct stat st0, st1, st2;
        fstat(fd, &st0);
        struct reader r;
        struct writer w;
        unsigned char c = 'l';
        memset(&r, 0, sizeof r);
        memset(&w, 0, sizeof w);
        pthread_t t;
        sleep_ms(200);
        if (writing)
        {
            w.fd = fds[1];
            w.buf = shape == 1 || shape == 5 ? (const void *)&c : (const void *)big;
            w.count = shape == 1 || shape == 5 ? 1 : 200000;
            pthread_create(&t, NULL, writer_main, &w);
        }
        else
        {
            r.fd = fds[0];
            r.count = 8;
            pthread_create(&t, NULL, reader_main, &r);
        }
        sleep_ms(500);
        fstat(fd, &st1);
        sleep_ms(500);
        if (shape == 3 || shape == 5) close(fds[0]);
        else if (shape == 4) close(fds[1]);
        else pthread_kill(t, SIGUSR1);
        pthread_join(t, NULL);
        fstat(fd, &st2);
        printf("L2 %s (asleep ~200, fstat ~700, ended ~1200): rv=%zd errno=%d atime %.0f/%.0f/%.0f mtime %.0f/%.0f/%.0f ctime %.0f/%.0f/%.0f\n",
               shapes[shape], writing ? w.rv : r.rv, writing ? w.error : r.error, ts_ms(st0.ATIM, base),
               ts_ms(st1.ATIM, base), ts_ms(st2.ATIM, base), ts_ms(st0.MTIM, base), ts_ms(st1.MTIM, base),
               ts_ms(st2.MTIM, base), ts_ms(st0.CTIM, base), ts_ms(st1.CTIM, base), ts_ms(st2.CTIM, base));
        if (shape != 3 && shape != 5) close(fds[0]);
        if (shape != 4) close(fds[1]);
    }
    // L3: a read restarted by SA_RESTART: signalled ~1200, fstat ~1450, data
    // ~1700.
    install_usr1(1);
    {
        clock_gettime(CLOCK_REALTIME, &base);
        int fds[2];
        pipe(fds);
        struct stat st1, st2;
        struct reader r;
        memset(&r, 0, sizeof r);
        r.fd = fds[0];
        r.count = 8;
        pthread_t t;
        sleep_ms(200);
        pthread_create(&t, NULL, reader_main, &r);
        sleep_ms(1000);
        pthread_kill(t, SIGUSR1);
        sleep_ms(250);
        fstat(fds[0], &st1);
        sleep_ms(250);
        write(fds[1], "abc", 3);
        pthread_join(t, NULL);
        fstat(fds[0], &st2);
        printf("L3 read restarted (asleep ~200, signalled ~1200, fstat ~1450, data ~1700): rv=%zd atime %.0f/%.0f\n",
               r.rv, ts_ms(st1.ATIM, base), ts_ms(st2.ATIM, base));
        close(fds[0]);
        close(fds[1]);
    }
    {
        clock_gettime(CLOCK_REALTIME, &base);
        int fds[2];
        pipe(fds);
        fill(fds[0], fds[1]);
        struct stat st1, st2;
        unsigned char c = 'r';
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = &c;
        w.count = 1;
        pthread_t t;
        sleep_ms(200);
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(1000);
        pthread_kill(t, SIGUSR1);
        sleep_ms(250);
        fstat(fds[1], &st1);
        sleep_ms(250);
        make_room(fds[0], 1);
        pthread_join(t, NULL);
        fstat(fds[1], &st2);
        printf("L3 write restarted (asleep ~200, signalled ~1200, fstat ~1450, room ~1700): rv=%zd mtime %.0f/%.0f\n",
               w.rv, ts_ms(st1.MTIM, base), ts_ms(st2.MTIM, base));
        close(fds[0]);
        close(fds[1]);
    }
}

// ---- N ------------------------------------------------------------------

// N1/N2: O_NONBLOCK set on the write end's description while a 200000-byte
// write sleeps, then 4096 bytes read: N1 into a full pipe (nothing in), N2
// into an empty one (65536 in).
static void section_n12(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int full = 1; full >= 0; full--)
    {
        int fds[2];
        pipe(fds);
        if (full) fill(fds[0], fds[1]);
        struct writer w;
        memset(&w, 0, sizeof w);
        w.fd = fds[1];
        w.buf = big;
        w.count = 200000;
        pthread_t t;
        pthread_create(&t, NULL, writer_main, &w);
        sleep_ms(50);
        int asleep = !atomic_load(&w.returned);
        set_nonblock(fds[1], 1);
        sleep_ms(50);
        int woke_on_flag = atomic_load(&w.returned);
        drain_bytes(fds[0], 4096);
        sleep_ms(100);
        int returned = atomic_load(&w.returned);
        if (!returned) close(fds[0]);
        pthread_join(t, NULL);
        printf("N%d write of 200000 into %s pipe, O_NONBLOCK set asleep=%d woke-on-flag=%d, then 4096 read: returned=%d rv=%zd errno=%d\n",
               full ? 1 : 2, full ? "a full" : "an empty", asleep, woke_on_flag, returned, w.rv, w.error);
        if (returned) close(fds[0]);
        close(fds[1]);
    }
}

#ifdef __linux__
// N3 (Linux; CAP_SYS_NICE): a sleeper woken with nothing to take, its
// description non-blocking by then. Sleeper and hog share CPU 0 under
// SCHED_FIFO, the hog higher: the hog makes room (or bytes) and takes it back
// before the sleeper runs.
static void section_n3(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int reading = 0; reading <= 1; reading++)
    {
        char seen[10][16];
        for (int trial = 0; trial < 10; trial++)
        {
            int fds[2];
            pipe(fds);
            if (!reading) fill(fds[0], fds[1]);
            struct h_sleeper h;
            memset(&h, 0, sizeof h);
            h.reading = reading;
            h.fd = reading ? fds[0] : fds[1];
            h.count = reading ? 16 : 4096;
            pthread_t t;
            pthread_create(&t, NULL, h_main, &h);
            while (!atomic_load(&h.started)) { }
            sleep_ms(30);
            set_nonblock(h.fd, 1);
            int hog = pin_fifo(20);
            if (hog != 0) { printf("N3 hog could not take SCHED_FIFO\n"); return; }
            unsigned char b[4096];
            if (reading)
            {
                write(fds[1], "z", 1);
                read(fds[0], b, 1);
            }
            else
            {
                drain_bytes(fds[0], 4096);
                write(fds[1], big, 4096);
            }
            struct sched_param sp = { .sched_priority = 0 };
            pthread_setschedparam(pthread_self(), SCHED_OTHER, &sp);
            sleep_ms(50);
            // Unstick a sleeper the spurious wake left asleep.
            if (reading) write(fds[1], "y", 1);
            else drain_bytes(fds[0], 8192);
            pthread_join(t, NULL);
            if (h.rv < 0) snprintf(seen[trial], sizeof seen[trial], "e%d", h.error);
            else snprintf(seen[trial], sizeof seen[trial], "%zd", h.rv);
            close(fds[0]);
            close(fds[1]);
        }
        printf("N3 %s woken with nothing to take, O_NONBLOCK set:", reading ? "a read" : "a 4096-byte write");
        for (int i = 0; i < 10; i++) printf(" %s", seen[i]);
        printf("\n");
    }
}
#endif

// N4: as N3 for a flavour with no SCHED_FIFO to hold the sleeper off the
// CPU (as section J): a helper process holding the pipe stops the process,
// makes room (or bytes) and takes it back through the same, now non-blocking,
// description, and continues it.
static void section_n4(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int reading = 0; reading <= 1; reading++)
    {
        char seen[10][16];
        for (int trial = 0; trial < 10; trial++)
        {
            int fds[2], go[2], ready[2];
            if (pipe(fds) != 0 || pipe(go) != 0 || pipe(ready) != 0) { perror("pipe"); exit(1); }
            if (!reading) fill(fds[0], fds[1]);
            pid_t parent = getpid();
            pid_t helper = fork();
            if (helper == 0)
            {
                alarm(10);
                char b = 1;
                if (write(ready[1], &b, 1) != 1) _exit(3);
                if (read(go[0], &b, 1) != 1) _exit(4);
                kill(parent, SIGSTOP);
                sleep_ms(50);
                unsigned char buf[4096];
                if (reading)
                {
                    write(fds[1], "z", 1);
                    sleep_ms(20);
                    read(fds[0], buf, 1);
                }
                else
                {
                    drain_bytes(fds[0], 4096);
                    sleep_ms(20);
                    write(fds[1], big, 4096);
                }
                sleep_ms(50);
                kill(parent, SIGCONT);
                _exit(0);
            }
            char b;
            if (read(ready[0], &b, 1) != 1) { perror("read"); exit(1); }
            struct reader r;
            struct writer w;
            memset(&r, 0, sizeof r);
            memset(&w, 0, sizeof w);
            pthread_t t;
            if (reading)
            {
                r.fd = fds[0];
                r.count = 16;
                pthread_create(&t, NULL, reader_main, &r);
            }
            else
            {
                w.fd = fds[1];
                w.buf = big;
                w.count = 4096;
                pthread_create(&t, NULL, writer_main, &w);
            }
            sleep_ms(50);
            set_nonblock(reading ? fds[0] : fds[1], 1);
            b = 1;
            if (write(go[1], &b, 1) != 1) { perror("write"); exit(1); }
            int status;
            waitpid(helper, &status, 0);
            sleep_ms(100);
            int returned = reading ? atomic_load(&r.returned) : atomic_load(&w.returned);
            if (!returned)
            {
                if (reading) write(fds[1], "y", 1);
                else drain_bytes(fds[0], 8192);
            }
            pthread_join(t, NULL);
            ssize_t rv = reading ? r.rv : w.rv;
            int error = reading ? r.error : w.error;
            if (rv < 0) snprintf(seen[trial], sizeof seen[trial], "%se%d", returned ? "" : "late:", error);
            else snprintf(seen[trial], sizeof seen[trial], "%s%zd", returned ? "" : "late:", rv);
            close(fds[0]); close(fds[1]);
            close(go[0]); close(go[1]); close(ready[0]); close(ready[1]);
        }
        printf("N4 %s woken with nothing to take, O_NONBLOCK set:", reading ? "a read" : "a 4096-byte write");
        for (int i = 0; i < 10; i++) printf(" %s", seen[i]);
        printf("\n");
    }
}

// N5: two sleepers, O_NONBLOCK set on their shared description, then room
// (or bytes) for one: what the other answers within 100 ms, if anything.
static void section_n5(void)
{
    signal(SIGPIPE, SIG_IGN);
    for (int reading = 0; reading <= 1; reading++)
    {
        char seen[10][32];
        for (int trial = 0; trial < 10; trial++)
        {
            int fds[2];
            pipe(fds);
            if (!reading) fill(fds[0], fds[1]);
            struct reader rs[2];
            struct writer ws[2];
            pthread_t ts[2];
            for (int i = 0; i < 2; i++)
            {
                memset(&rs[i], 0, sizeof rs[i]);
                memset(&ws[i], 0, sizeof ws[i]);
                if (reading)
                {
                    rs[i].fd = fds[0];
                    rs[i].count = 1;
                    pthread_create(&ts[i], NULL, reader_main, &rs[i]);
                }
                else
                {
                    ws[i].fd = fds[1];
                    ws[i].buf = big;
                    ws[i].count = 4096;
                    pthread_create(&ts[i], NULL, writer_main, &ws[i]);
                }
                sleep_ms(30);
            }
            sleep_ms(50);
            set_nonblock(reading ? fds[0] : fds[1], 1);
            if (reading) write(fds[1], "z", 1);
            else drain_bytes(fds[0], 4096);
            sleep_ms(100);
            char *p = seen[trial];
            for (int i = 0; i < 2; i++)
            {
                int returned = reading ? atomic_load(&rs[i].returned) : atomic_load(&ws[i].returned);
                ssize_t rv = reading ? rs[i].rv : ws[i].rv;
                int error = reading ? rs[i].error : ws[i].error;
                if (!returned) p += sprintf(p, "%sasleep", i ? "/" : "");
                else if (rv < 0) p += sprintf(p, "%se%d", i ? "/" : "", error);
                else p += sprintf(p, "%s%zd", i ? "/" : "", rv);
            }
            // Unstick whatever still sleeps.
            if (reading) write(fds[1], "yy", 2);
            else drain_bytes(fds[0], 8192);
            for (int i = 0; i < 2; i++) pthread_join(ts[i], NULL);
            close(fds[0]);
            close(fds[1]);
        }
        printf("N5 two %s, O_NONBLOCK set, room for one:", reading ? "1-byte readers" : "4096-byte writers");
        for (int i = 0; i < 10; i++) printf(" %s", seen[i]);
        printf("\n");
    }
}

// N6: a write of at most PIPE_BUF bytes asleep on a full pipe, O_NONBLOCK
// set or not, then a read too small to make room for it (Linux: 50 bytes of a
// 4096-byte write, no slot freed; Darwin: 50 of a 512-byte write): whether it
// returns within 100 ms, and what.
static void section_n6(void)
{
    signal(SIGPIPE, SIG_IGN);
#ifdef __linux__
    size_t size = 4096;
#else
    size_t size = 512;
#endif
    for (int nonblocking = 0; nonblocking <= 1; nonblocking++)
    {
        char seen[5][24];
        for (int trial = 0; trial < 5; trial++)
        {
            int fds[2];
            pipe(fds);
            fill(fds[0], fds[1]);
            struct writer w;
            memset(&w, 0, sizeof w);
            w.fd = fds[1];
            w.buf = big;
            w.count = size;
            pthread_t t;
            pthread_create(&t, NULL, writer_main, &w);
            sleep_ms(50);
            if (nonblocking) set_nonblock(fds[1], 1);
            drain_bytes(fds[0], 50);
            sleep_ms(100);
            int returned = atomic_load(&w.returned);
            if (!returned) drain_bytes(fds[0], 8192);
            pthread_join(t, NULL);
            if (w.rv < 0) snprintf(seen[trial], sizeof seen[trial], "%se%d", returned ? "" : "late:", w.error);
            else snprintf(seen[trial], sizeof seen[trial], "%s%zd", returned ? "" : "late:", w.rv);
            close(fds[0]);
            close(fds[1]);
        }
        printf("N6 a %zu-byte write, O_NONBLOCK %s, 50 bytes read:", size, nonblocking ? "set" : "not set");
        for (int i = 0; i < 5; i++) printf(" %s", seen[i]);
        printf("\n");
    }
}

int main(int argc, char **argv)
{
    alarm(600);
    main_thread = pthread_self();
    signal(SIGPIPE, SIG_IGN);
    setvbuf(stdout, NULL, _IONBF, 0);
    const char *only = argc > 1 ? argv[1] : "ABCDEFGHIJKLN";
    if (strchr(only, 'A')) section_a();
    if (strchr(only, 'B')) section_b();
    if (strchr(only, 'C')) section_c();
    if (strchr(only, 'D')) section_d();
#ifdef __linux__
    if (strchr(only, 'H')) section_h();
#endif
    if (strchr(only, 'E')) section_e();
    if (strchr(only, 'F')) section_f();
    if (strchr(only, 'G')) section_g();
    if (strchr(only, 'I')) section_i();
    if (strchr(only, 'J')) section_j();
    if (strchr(only, 'K')) section_k();
    if (strchr(only, 'L')) section_l();
    if (strchr(only, 'N')) section_n12();
    if (strchr(only, 'N')) section_n4();
    if (strchr(only, 'N')) section_n5();
    if (strchr(only, 'N')) section_n6();
#ifdef __linux__
    if (strchr(only, 'N')) section_n3();
#endif
    return 0;
}
