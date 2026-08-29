# Differential fuzzer: the socket/epoll model against the real Linux kernel

The readiness-delivery model in `EmulatedKernel` was established by hand-written
probes, one measured row at a time, and its guarantee today is "matches those
rows". This fuzzer upgrades that to "matches the kernel on generated operation
sequences": the reference implementation exists (real Linux epoll, reachable via
`container`), so random sequences run on both and the transcripts must agree.

## Options considered

### 1. Comparison altitude

**(a) Kernel-level.** One C interpreter (`harness.c`, compiled once per run in
the `gcc:14` container) executes op sequences against real sockets and epoll;
the F# side executes the same sequences against `EmulatedKernel` functions
directly (`createSocket`, `connectSocket`, `acceptConnection`, `closeFd`,
`dup`, `changeSocketEventRegistration`, `deliverSocketEvents`). Fast — the
whole batch is one container run — and it compares exactly the model that took
ten review rounds: ready-list membership, order, consumption, truncation, and
the per-phase masks.

**(b) Guest-level.** Generate a raw-P/Invoke C# guest per sequence, run it
under PawPrint and on real Linux .NET, compare transcripts. End-to-end
(includes the PAL conversion and the native handlers), but two orders of
magnitude slower per sequence and needs a .NET container image.

**Chose (a)** (ratified 2026-08-22, with the guest-level rig noted as a
possible later addition over corpus-sampled sequences). What (a) does *not*
cover, deliberately: the native layer (`SystemNative_WaitForSocketEvents`
buffer writes, the PAL's HUP fold, the Bind/Listen handlers) and the park/wake
sweep (a timeout-0 wait never parks) — those keep their existing hand-written
guests. Bind/listen semantics live in the native handler rather than in
`EmulatedKernel`, so the fuzzer's `lstn` op is the trivially-conflict-free
composite (loopback, distinct fixed ports, backlog 8), constructed the way
`TestSocketEventDelivery`'s `addListener` does; bind-conflict semantics are
out of the fuzzed vocabulary.

### 2. Suite integration

**(a) Corpus + gated live.** The live differential loop is an NUnit test gated
on `PAWPRINT_SOCKET_FUZZ=1` (`Assert.Ignore` otherwise — the container only
exists on the dev box). Sequences whose transcripts agree can be written out
as a corpus (`socketFuzzCorpus/*.txt`, embedded resources); a deterministic
replay test runs the corpus against `EmulatedKernel` in CI with no container.

**(b) Live-only.** Nothing checked in beyond harness and generator; CI never
exercises any of it.

**Chose (a)** (ratified 2026-08-22): determinism in CI, and a resolved
divergence becomes a permanent regression row.

## The op language

One sequence per line, ops space-separated, slots naming fds on both sides
(slot→fd maps are per-side; raw fd numbers never appear in a transcript):

| op | meaning |
| --- | --- |
| `sock:s` | new nonblocking TCP stream socket in slot `s` |
| `lstn:s` | bind slot `s` to loopback (real: port 0; emulated: 20000+k, below the ephemeral range so implicit client binds cannot collide) and listen backlog 8 |
| `conn:c:l` | nonblocking `connect(2)` of slot `c` toward listener slot `l` |
| `conndead:c` | connect toward loopback:1, where nothing ever listens |
| `acpt:l:s2` | nonblocking accept from `l` into fresh slot `s2` |
| `close:s` | `close(2)` |
| `dup:s:s2` | `dup(2)` |
| `port:p` | `epoll_create1` |
| `add:p:t:m` / `mod:p:t:m` | `epoll_ctl` ADD/MOD, interest `m` in `SA_*` bits (0..0x1F, translated 1:1 to epoll bits, `EPOLLET` always ORed in as the PAL does), data = `t` |
| `del:p:t` | `epoll_ctl` DEL |
| `wait:p:n` | `epoll_wait` timeout 0, maxevents `n` |

Transcript: one token per op — `ok`, an errno name (`UnixError` case names and
glibc's `strerrorname_np` agree by construction), or for `wait` a batch
`[data/IN+OUT,...]` with mask bits in canonical IN,OUT,RDHUP,HUP,ERR order.
The comparison sits *below* the PAL's HUP fold on both sides: raw epoll bits
against `EpollReadiness`.

`connect(2)` needs no dialect translation: `SystemNative_Connect` is a
passthrough (EINTR retry plus errno rename), and `connectSocket` models it,
so raw connect in the harness speaks the same language — including the
completion-reporting retry (SUCCESS) and the refusal-delivering retry
(ECONNREFUSED + reset), which the generator exercises deliberately.

## Determinism on the real side

The emulated kernel signals synchronously at the producing op, so the harness
must make the real side settle to the same convention: `usleep` after every op
with asynchronous effects (connect, close, accept), so that op order *is* edge
order. Each sequence then runs three times (forked children, isolated fd
tables); disagreement across the three marks the sequence `unstable` and it is
excluded from comparison (and counted — a high unstable rate means the settle
is too short, and the live test asserts a bound on it).

## Refusals are skips, not failures — with two exceptions

The emulated executor wraps each op; a `failwith` from the kernel is the
model's explicit "outside modelled scope" and the sequence is skipped and
counted. Two message classes are *not* skips: anything containing
"interpreter bug", and any nonempty `checkInvariants` answer (both kernels'
invariants are asserted after every op) — those fail the run, because a
generated sequence that reaches one is a finding in its own right.

The generator is phase-aware (constructive generation, not filtering): it
tracks a shadow phase per slot so that most sequences stay inside the modelled
envelope — e.g. it never closes a listener with a nonempty shadow accept
queue (a modelled refusal), never targets a port with `add` (nested epoll is
refused), and biases toward registration/wait-rich sequences. Its weights are
themselves drawn per-sequence from the seed, so the distribution is fuzzed
too. The live test asserts the observed distribution (op coverage, nonempty
batches, skip and unstable rates) rather than printing it.

## How to run

```
PAWPRINT_SOCKET_FUZZ=1 nix develop -c dotnet test WoofWare.PosixKernel.Test/WoofWare.PosixKernel.Test.fsproj \
  --filter "Name~SocketFuzzLive" --verbosity normal
```

Optional: `PAWPRINT_SOCKET_FUZZ_SEQUENCES=<n>` (default in the test),
`PAWPRINT_SOCKET_FUZZ_SEED=<int>` (default fixed; the failure message prints
the seed and the offending sequence, which is a one-line repro),
`PAWPRINT_SOCKET_FUZZ_WRITE_CORPUS=<path>` to append agreeing rows for
checking in to `socketFuzzCorpus/`.

The corpus replay test (`SocketFuzzCorpus`) runs everywhere, container or not.

The fuzzer lives in `WoofWare.PosixKernel.Test` as of the extraction's stage 20:
its emulated side is `UnixSystem`, and comparing that against a real Linux kernel
is a claim about the library rather than about its client. The `PAWPRINT_`
variable names are unchanged -- they are the documented interface, and renaming
them would break every invocation anyone has recorded.
