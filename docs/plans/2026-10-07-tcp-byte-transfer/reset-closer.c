// Whether the end that closed still holds its port, after a reset against a
// FIN. The connecting socket closes, with 10 bytes the accepted one wrote to
// it unread (it sends a reset) or with nothing unread (a FIN); the accepted
// socket stays open. Then a fresh socket, without SO_REUSEADDR, binds the
// closed socket's exact endpoint.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/rc reset-closer.c && /tmp/rc
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/reset-closer.c && /tmp/p'
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

// The closer is the connecting socket; the accepted one survives.
static void run(char how)
{
    int l = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in a = at(0);
    bind(l, (struct sockaddr*)&a, sizeof a); listen(l, 4);
    struct sockaddr_in d = at(port_of(l));
    int c = socket(AF_INET, SOCK_STREAM, 0);
    connect(c, (struct sockaddr*)&d, sizeof d);
    int p = accept(l, NULL, NULL);
    int cport = port_of(c);
    if (how == 'r') { write(p, "0123456789", 10); usleep(20000); }
    close(c);
    usleep(50000);
    int n = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in k = at(cport);
    printf("closer=c how=%c bind_closer_endpoint=%s\n", how, res(bind(n, (struct sockaddr*)&k, sizeof k)));
    close(n); close(p); close(l);
}

int main(void) { run('r'); run('f'); return 0; }
