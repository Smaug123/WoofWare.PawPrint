// Whether listen(2) on a socket already listening answers EADDRINUSE while a
// connection it accepted holds the same port: the accepted socket open, or
// closed with its FIN made first (so that its end still holds the endpoint),
// or closed after the client's FIN (so that it does not); the listener with
// and without SO_REUSEADDR.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/relisten-held relisten-held.c && /tmp/relisten-held
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/relisten-held.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root)
// twice and Darwin 27.0.0 arm64 (uid 501) twice, identical each time; one
// run of each is beside this. Every re-listen answers 0 on both: Linux's
// `listen` of a socket already listening only updates its backlog.

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>
static void settle(void){usleep(30000);}
static int port_of(int fd){struct sockaddr_in a;socklen_t l=sizeof a;getsockname(fd,(struct sockaddr*)&a,&l);return ntohs(a.sin_port);}
int main(void){
 struct utsname u; uname(&u); printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);
 for(int reuse=0;reuse<2;reuse++)for(int child=0;child<3;child++){
  int l=socket(AF_INET,SOCK_STREAM,0);int one=1;if(reuse)setsockopt(l,SOL_SOCKET,SO_REUSEADDR,&one,sizeof one);
  struct sockaddr_in a;memset(&a,0,sizeof a);a.sin_family=AF_INET;a.sin_addr.s_addr=htonl(0x7f000001);
  bind(l,(struct sockaddr*)&a,sizeof a);listen(l,4);a.sin_port=htons(port_of(l));
  int c=socket(AF_INET,SOCK_STREAM,0);connect(c,(struct sockaddr*)&a,sizeof a);settle();int p=accept(l,0,0);
  /* child 0: p open; 1: p closed first (FIN_WAIT2 orphan); 2: p closed after c shut WR (passive, released) */
  if(child==2){shutdown(c,SHUT_WR);settle();}
  if(child>=1){close(p);settle();}
  int r=listen(l,8);int e=errno;
  printf("listener-reuse=%d child=%s relisten=%s\n",reuse,child==0?"open":child==1?"closed-active":"closed-passive",r==0?"ok":e==EADDRINUSE?"EADDRINUSE":strerror(e));
  if(child==0)close(p);
  close(c);close(l);settle();
 }
 return 0;}
