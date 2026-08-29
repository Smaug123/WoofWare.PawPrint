// Where each field of `struct sockaddr_in` and `struct sockaddr_in6` actually
// sits, and how big each struct is.
//
// PawPrint hardcodes these as `SockaddrOffsets` on the interpreter's side; the
// question this answers is whether any of them varies by platform, which decides
// whether the library can own them as constants.
#include <netinet/in.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/socket.h>

int main(void)
{
    printf("sockaddr.sa_family off=%zu size=%zu\n",
           offsetof(struct sockaddr, sa_family), sizeof(((struct sockaddr *)0)->sa_family));
    printf("sockaddr_in size=%zu sin_family=%zu sin_port=%zu sin_addr=%zu\n",
           sizeof(struct sockaddr_in),
           offsetof(struct sockaddr_in, sin_family),
           offsetof(struct sockaddr_in, sin_port),
           offsetof(struct sockaddr_in, sin_addr));
    printf("sockaddr_in6 size=%zu sin6_family=%zu sin6_port=%zu sin6_flowinfo=%zu sin6_addr=%zu sin6_scope_id=%zu\n",
           sizeof(struct sockaddr_in6),
           offsetof(struct sockaddr_in6, sin6_family),
           offsetof(struct sockaddr_in6, sin6_port),
           offsetof(struct sockaddr_in6, sin6_flowinfo),
           offsetof(struct sockaddr_in6, sin6_addr),
           offsetof(struct sockaddr_in6, sin6_scope_id));
    printf("in6_addr size=%zu\n", sizeof(struct in6_addr));
    printf("sockaddr_storage size=%zu\n", sizeof(struct sockaddr_storage));
    return 0;
}
