// How many blocking TCP connects a Darwin or Linux listener with a small
// backlog lets complete, nothing accepting, and how each ends. A connect that
// sleeps is ended by a 1.5 s watchdog signal.
#define _GNU_SOURCE
#include <errno.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>
static void on_usr1(int s) { (void)s; }
static void *dog(void *p) { usleep(1500 * 1000); pthread_kill(*(pthread_t *)p, SIGUSR1); return NULL; }
static double now_ms(void) { struct timespec ts; clock_gettime(CLOCK_MONOTONIC, &ts); return ts.tv_sec * 1e3 + ts.tv_nsec / 1e6; }
int main(int argc, char **argv)
{
    setvbuf(stdout, NULL, _IONBF, 0);
    int backlog = argc > 1 ? atoi(argv[1]) : 1;
    struct sigaction sa; memset(&sa, 0, sizeof sa); sa.sa_handler = on_usr1; sigaction(SIGUSR1, &sa, NULL);
    struct sockaddr_in a; memset(&a, 0, sizeof a); a.sin_family = AF_INET; a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    int l = socket(AF_INET, SOCK_STREAM, 0); bind(l, (struct sockaddr *)&a, sizeof a);
    socklen_t len = sizeof a; getsockname(l, (struct sockaddr *)&a, &len); listen(l, backlog);
    for (int i = 0; i < 8; i++) {
        int c = socket(AF_INET, SOCK_STREAM, 0);
        pthread_t self = pthread_self(), t; pthread_create(&t, NULL, dog, &self);
        double t0 = now_ms();
        int r = connect(c, (struct sockaddr *)&a, sizeof a); int e = errno;
        double took = now_ms() - t0;
        pthread_join(t, NULL);
        printf("listen(%d), connect #%d: ret=%d errno=%s took_ms=%.0f\n", backlog, i + 1, r, r < 0 ? strerror(e) : "-", took);
    }
    return 0;
}
