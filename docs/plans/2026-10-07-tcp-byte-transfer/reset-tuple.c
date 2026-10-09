// Whether a connection a reset has reached still occupies its four-tuple,
// against a FIN. The connecting socket `c`, with SO_REUSEADDR (and with
// `explicit=1`, bound to an explicit port), connects to a listener that stays
// open; the accepted end closes, after `c` wrote 10 bytes it never read (a
// reset reaches `c`) or with nothing unread (a FIN). Then a fresh socket with
// SO_REUSEADDR binds `c`'s exact endpoint and connects to the listener: the
// same four-tuple.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/rt reset-tuple.c && /tmp/rt
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/reset-tuple.c && /tmp/p'
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

static void run(char how, int explicit_port)
{
    int one = 1;
    int l = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a = at(0);
    bind(l, (struct sockaddr*)&a, sizeof a); listen(l, 4);
    struct sockaddr_in d = at(port_of(l));
    int c = socket(AF_INET, SOCK_STREAM, 0);
    setsockopt(c, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    if (explicit_port) { struct sockaddr_in b = at(48200 + (how == 'r')); bind(c, (struct sockaddr*)&b, sizeof b); }
    connect(c, (struct sockaddr*)&d, sizeof d);
    int p = accept(l, NULL, NULL);
    int cport = port_of(c);
    if (how == 'r') { write(c, "0123456789", 10); usleep(20000); }
    close(p);
    usleep(50000);
    int n = socket(AF_INET, SOCK_STREAM, 0);
    setsockopt(n, SOL_SOCKET, SO_REUSEADDR, &one, sizeof one);
    struct sockaddr_in k = at(cport);
    int bn = bind(n, (struct sockaddr*)&k, sizeof k);
    const char *bns = strdup(res(bn));
    int cn = connect(n, (struct sockaddr*)&d, sizeof d);
    printf("how=%c explicit=%d bind=%s connect_same_tuple=%s\n", how, explicit_port, bns, res(cn));
    close(n); close(c); close(l);
}

int main(void)
{
    run('r', 0); run('r', 1); run('f', 0); run('f', 1);
    return 0;
}
