// What a reset leaves of a connected socket's binding and options, against a
// FIN. A loopback pair is made, the listener closed, and one end (`c`, the
// connecting socket, or `s`, the accepted one) survives while the other
// closes: after the survivor wrote 10 bytes the closer never read (a reset
// reaches the survivor), or with nothing unread (a FIN). `locked=1` binds the
// listener and the connecting socket to explicit ports first. Then:
//   so_error            what getsockopt(SO_ERROR) read on the survivor;
//   setsockopt          setsockopt(survivor, SO_REUSEADDR, 1);
//   bind                a fresh socket binding the survivor's exact endpoint;
//   bind_reuse          another, with SO_REUSEADDR, binding it after that;
//   getsockname_port_same  whether the survivor still reports its port.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/rb reset-binding.c && /tmp/rb
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/reset-binding.c && /tmp/p'
#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static int port_of(int fd) { struct sockaddr_in a; socklen_t l = sizeof a; getsockname(fd, (struct sockaddr*)&a, &l); return ntohs(a.sin_port); }
static struct sockaddr_in at(int port) { struct sockaddr_in a; memset(&a, 0, sizeof a); a.sin_family = AF_INET; a.sin_port = htons(port); a.sin_addr.s_addr = htonl(0x7f000001); return a; }
static const char *res(int r) { static char b[64]; if (r == 0) return "ok"; snprintf(b, sizeof b, "-1 %s", strerror(errno)); return b; }

// survivor: 'c' client or 's' server end survives; how: 'r' reset (closer had unread), 'f' fin; locked: client bound explicitly
static void run(char survivor, char how, int locked)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    int one = 1;
    static int next = 47100;
    struct sockaddr_in a = at(locked ? next++ : 0);
    bind(l, (struct sockaddr*)&a, sizeof a); listen(l, 4);
    int lport = port_of(l);
    int c = socket(AF_INET, SOCK_STREAM, 0);
    if (locked) { struct sockaddr_in b = at(next++); if (bind(c, (struct sockaddr*)&b, sizeof b)) perror("bind c"); }
    struct sockaddr_in d = at(lport);
    connect(c, (struct sockaddr*)&d, sizeof d);
    int p = accept(l, NULL, NULL);
    close(l);
    int keep = survivor == 'c' ? c : p, gone = survivor == 'c' ? p : c;
    if (how == 'r') { write(keep, "0123456789", 10); usleep(20000); }
    close(gone);
    usleep(50000);
    int kport = port_of(keep);
    int e = 0; socklen_t el = sizeof e; getsockopt(keep, SOL_SOCKET, SO_ERROR, &e, &el);
    int so = setsockopt(keep, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    const char *sos = strdup(res(so));
    int n = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in k = at(kport);
    int bn = bind(n, (struct sockaddr*)&k, sizeof k);
    const char *bns = strdup(res(bn));
    int n2 = socket(AF_INET, SOCK_STREAM, 0);
    setsockopt(n2, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    int br = bind(n2, (struct sockaddr*)&k, sizeof k);
    printf("survivor=%c how=%c locked=%d so_error=%d setsockopt=%s bind=%s bind_reuse=%s getsockname_port_same=%d\n",
           survivor, how, locked, e, sos, bns, res(br), port_of(keep) == kport);
    close(n); close(n2); close(keep);
}

int main(void)
{
    for (int s = 0; s < 2; s++)
        for (int h = 0; h < 2; h++)
            for (int lk = 0; lk < 2; lk++)
                run(s ? 's' : 'c', h ? 'f' : 'r', lk);
    return 0;
}
