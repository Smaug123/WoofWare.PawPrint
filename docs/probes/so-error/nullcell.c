// With a null value buffer, does getsockopt(SO_ERROR) read the length cell,
// and does a call that faults on the length cell still take a pending refusal?
//
// Both flavours; outputs beside this file. Run as soerror.c is.
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/mman.h>
#include <netinet/in.h>
#include <arpa/inet.h>
static struct sockaddr_in da;
static void *unmapped;
static int refused(void) {
    int c = socket(AF_INET, SOCK_STREAM, 0); fcntl(c, F_SETFL, fcntl(c, F_GETFL) | O_NONBLOCK);
    connect(c, (struct sockaddr *)&da, sizeof da); usleep(100000); return c;
}
static void row(const char *label, void *val, void *len, int option) {
    int c = refused(); errno = 0;
    int rc = getsockopt(c, SOL_SOCKET, option, val, len);
    int e = rc ? errno : 0;
    int v = -7; socklen_t l = 4; getsockopt(c, SOL_SOCKET, SO_ERROR, &v, &l);
    printf("  %-44s rc=%d errno=%d, then SO_ERROR=%d\n", label, rc, e, v);
    close(c);
}
int main(void) {
    unmapped = mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
    memset(&da, 0, sizeof da); da.sin_family = AF_INET; da.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    int t = socket(AF_INET, SOCK_STREAM, 0); bind(t, (struct sockaddr *)&da, sizeof da);
    socklen_t sl = sizeof da; getsockname(t, (struct sockaddr *)&da, &sl); close(t);
    int v; socklen_t l4 = 4, lneg = (socklen_t)-1;
    row("SO_ERROR, null value, null length", NULL, NULL, SO_ERROR);
    row("SO_ERROR, null value, unmapped length", NULL, unmapped, SO_ERROR);
    l4 = 4; row("SO_ERROR, null value, length 4", NULL, &l4, SO_ERROR);
    lneg = (socklen_t)-1; row("SO_ERROR, null value, length -1", NULL, &lneg, SO_ERROR);
    row("SO_ERROR, unmapped value, null length", unmapped, NULL, SO_ERROR);
    row("SO_ERROR, real value, null length", &v, NULL, SO_ERROR);
    row("SO_ERROR, real value, unmapped length", &v, unmapped, SO_ERROR);
    row("SO_REUSEADDR, null value, null length", NULL, NULL, SO_REUSEADDR);
    row("SO_REUSEADDR, null value, unmapped length", NULL, unmapped, SO_REUSEADDR);
    return 0;
}
