// Does the socket `accept(2)` returns inherit the listener's O_NONBLOCK?
//
// The classic BSD/Linux divergence, and the reason CoreCLR's
// `SystemNative_Accept` calls `SystemNative_FcntlSetIsNonBlocking(accepted, 0)`
// under `#if !defined(__linux__)` ("On macOS and FreeBSD new socket inherits
// flags from accepting fd").
//
// Prints one row per listener setting:
//
//     listener_nonblock=<0|1> accepted_nonblock=<0|1>
//
// Build: cc -o accept-inherits-nonblock accept-inherits-nonblock.c
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

static int nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) {
        perror("fcntl(F_GETFL)");
        return -1;
    }
    return (flags & O_NONBLOCK) ? 1 : 0;
}

static int probe(int listenerNonBlocking)
{
    int listener = socket(AF_INET, SOCK_STREAM, 0);
    if (listener < 0) { perror("socket(listener)"); return 1; }

    struct sockaddr_in addr;
    memset(&addr, 0, sizeof addr);
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = 0;

    if (bind(listener, (struct sockaddr*)&addr, sizeof addr) < 0) { perror("bind"); return 1; }
    if (listen(listener, 8) < 0) { perror("listen"); return 1; }

    socklen_t len = sizeof addr;
    if (getsockname(listener, (struct sockaddr*)&addr, &len) < 0) { perror("getsockname"); return 1; }

    if (listenerNonBlocking) {
        int flags = fcntl(listener, F_GETFL, 0);
        if (fcntl(listener, F_SETFL, flags | O_NONBLOCK) < 0) { perror("fcntl(F_SETFL)"); return 1; }
    }

    int client = socket(AF_INET, SOCK_STREAM, 0);
    if (client < 0) { perror("socket(client)"); return 1; }
    if (connect(client, (struct sockaddr*)&addr, sizeof addr) < 0) { perror("connect"); return 1; }

    struct sockaddr_in peer;
    socklen_t peerLen = sizeof peer;
    int accepted = accept(listener, (struct sockaddr*)&peer, &peerLen);
    if (accepted < 0) { perror("accept"); return 1; }

    printf("listener_nonblock=%d accepted_nonblock=%d\n", listenerNonBlocking, nonblocking(accepted));

    close(accepted);
    close(client);
    close(listener);
    return 0;
}

int main(void)
{
    if (probe(0) != 0) return 1;
    if (probe(1) != 0) return 1;
    return 0;
}
