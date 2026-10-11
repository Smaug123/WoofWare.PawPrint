// Whether a change of SO_REUSEADDR on a connected socket, made once its end
// is in TIME_WAIT or FIN_WAIT2, changes what a bind of the end's endpoint
// answers, with the socket left open or then closed. The beside
// bind-held-endpoint.c measures the flag set before the connect.
//
// c connects to a listener (closed once c's peer p is accepted), with
// SO_REUSEADDR set first or not (`reuse 0->1` or `1->0`). Then c
// shutdown(WR), and in a `tw` row p shutdown(WR) too, so that c is in
// TIME_WAIT, and in an `fw2` row not, so that c is in FIN_WAIT2. Then c's
// flag is flipped (the setsockopt's answer is printed), c is closed or not,
// and a fresh socket binds c's endpoint without and with SO_REUSEADDR.
//
// Build and run, from this directory:
//   Darwin: clang -Wall -O1 -o /tmp/bind-late-reuse bind-late-reuse.c && /tmp/bind-late-reuse
//   Linux:  container run --rm -v "$PWD":/probe debian:trixie sh -c 'apt-get update -qq >/dev/null 2>&1 && apt-get install -y -qq gcc libc6-dev >/dev/null 2>&1 && gcc -Wall -O1 -o /tmp/p /probe/bind-late-reuse.c && /tmp/p'
//
// Measured 2026-10-10 on Linux 6.18.5 aarch64 (Apple's container VM, root)
// twice and Darwin 27.0.0 arm64 (uid 501) twice, identical each time; one run
// of each is beside this. On Linux a bind is judged by the flag the end had
// when it entered TIME_WAIT, whether the socket is then open or closed,
// and in FIN_WAIT2 by the flag as it stands (or stood at the close). Darwin's
// setsockopt answers EINVAL in TIME_WAIT, and its binds ignore the end's
// flag throughout.

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/utsname.h>
#include <unistd.h>
static void settle(void){usleep(30000);}
static struct sockaddr_in at(int port){struct sockaddr_in a;memset(&a,0,sizeof a);a.sin_family=AF_INET;a.sin_port=htons(port);a.sin_addr.s_addr=htonl(0x7f000001);return a;}
static int port_of(int fd){struct sockaddr_in a;socklen_t l=sizeof a;getsockname(fd,(struct sockaddr*)&a,&l);return ntohs(a.sin_port);}
static void setr(int fd,int v){setsockopt(fd,SOL_SOCKET,SO_REUSEADDR,&v,sizeof v);}
static const char*cand(int reuse,int port){static char b[4][48];static int i;i=(i+1)%4;int n=socket(AF_INET,SOCK_STREAM,0);if(reuse)setr(n,1);struct sockaddr_in a=at(port);int r=bind(n,(struct sockaddr*)&a,sizeof a);snprintf(b[i],48,"%s",r==0?"ok":errno==EADDRINUSE?"EADDRINUSE":strerror(errno));close(n);return b[i];}
int main(void){
 struct utsname u; uname(&u); printf("#\t%s %s %s\n", u.sysname, u.release, u.machine);
 /* state: tw = c SHUT_WR then p SHUT_WR (c TIME_WAIT); fw2 = c SHUT_WR only */
 const char*st[]={"tw","fw2"};
 for(int s=0;s<2;s++)for(int before=0;before<2;before++)for(int closeit=0;closeit<2;closeit++){
  int l=socket(AF_INET,SOCK_STREAM,0);struct sockaddr_in a=at(0);bind(l,(struct sockaddr*)&a,sizeof a);listen(l,4);
  int c=socket(AF_INET,SOCK_STREAM,0);if(before)setr(c,1);struct sockaddr_in d=at(port_of(l));connect(c,(struct sockaddr*)&d,sizeof d);settle();
  int p=accept(l,0,0);close(l);int port=port_of(c);
  shutdown(c,SHUT_WR);settle(); if(s==0){shutdown(p,SHUT_WR);settle();}
  int sr=-1; { int v=!before; sr=setsockopt(c,SOL_SOCKET,SO_REUSEADDR,&v,sizeof v); }
  if(closeit){close(c);settle();}
  printf("%s reuse %d->%d (setsockopt=%d) c %s: bind=%s bind+reuse=%s\n",st[s],before,!before,sr,closeit?"closed":"open",cand(0,port),cand(1,port));
  if(!closeit)close(c);
  close(p); settle();
 }
 return 0;}
