# EntropyStreamsDefault.cs, reproduced outside PawPrint: `python3 entropy-streams-default.py new 0` prints the
# five rows TestImpureCases pins (srand48 seeded with 0), and `old` the rows the separate C-library stream gave.
import sys
M=(1<<64)-1
G=0x9E3779B97F4A7C15
def mix(z):
    z=((z^(z>>30))*0xBF58476D1CE4E5B9)&M
    z=((z^(z>>27))*0x94D049BB133111EB)&M
    return z^(z>>31)
class Pool:
    # EntropyPool.take: output k of a draw has state start+(k+1)*G; the pool moves ceil(n/8) outputs.
    def __init__(s,seed): s.state=seed
    def take(s,n):
        out=bytearray()
        k=0
        while len(out)<n:
            o=mix((s.state+(k+1)*G)&M)
            out+=o.to_bytes(8,'little'); k+=1
        s.state=(s.state+((n+7)//8)*G)&M
        return bytes(out[:n])
class Lrand:
    def __init__(s,seed): s.x=((seed&0xFFFFFFFF)<<16)|0x330E
    def next(s):
        s.x=(0x5DEECE66D*s.x+0xB)&((1<<48)-1); return s.x>>17
def mask(l,n):
    out=bytearray(); num=0
    for i in range(n):
        if i%4==0: num=l.next()
        out.append(num&0xFF); num>>=8
    return bytes(out)
def xor(a,b): return bytes(x^y for x,y in zip(a,b))
def guid(b):
    b=bytearray(b); b[7]=(b[7]&0x0F)|0x40; b[8]=(b[8]&0x3F)|0x80; return bytes(b)
def rotl(x,k): return ((x<<k)|(x>>(64-k)))&M
def xoshiro_nexts(seed32,count):
    s=[int.from_bytes(seed32[8*i:8*i+8],'little') for i in range(4)]
    res=[]
    while len(res)<count:
        s0,s1,s2,s3=s
        r=(rotl((s1*5)&M,7)*9)&M; t=(s1<<17)&M
        s2^=s0; s3^=s1; s1^=s2; s0^=s3; s2^=t; s3=rotl(s3,45)
        s=[s0,s1,s2,s3]
        v=r>>33
        if v!=0x7FFFFFFF: res.append(v)
    return res
def le32(v): return v.to_bytes(4,'little')
POOL_SEED=0x243F6A8885A308D3
def old_model():
    p=Pool(POOL_SEED); c=Pool(0x9E3779B97F4A7C15)
    # the old C-library stream is NonCryptoRandom.drawBytes: splitmix64 *stepped* (state += G, output mix(state)), which is the same as Pool.take
    r1=guid(p.take(16))
    seed=c.take(32); a,b=xoshiro_nexts(seed,2); r2=le32(a)+le32(b)
    r3=p.take(24); r4=c.take(24); r5=guid(p.take(16))
    return [x.hex() for x in (r1,r2,r3,r4,r5)]
def new_model(t):
    p=Pool(POOL_SEED); l=Lrand(t)
    r1=guid(p.take(16))
    seed=xor(p.take(32),mask(l,32)); a,b=xoshiro_nexts(seed,2); r2=le32(a)+le32(b)
    r3=p.take(24); r4=xor(p.take(24),mask(l,24)); r5=guid(p.take(16))
    return [x.hex() for x in (r1,r2,r3,r4,r5)]
if __name__=='__main__':
    if sys.argv[1]=='old': print('\n'.join(old_model()))
    else: print('\n'.join(new_model(int(sys.argv[2]))))
