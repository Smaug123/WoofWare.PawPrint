# Probes for the `SystemNative_Poll` plan

Reproduce (from the repository root):

```bash
# Darwin (this host)
cc -o /tmp/p pollmask.c && /tmp/p          # likewise pollmulti.c, pollgaps.c

# Linux, via Apple's `container` CLI
container run --rm -v "$PWD:/probe" gcc:latest \
    bash -c 'cc -o /tmp/p /probe/pollmask.c && /tmp/p'
```

Measured on Darwin 25.6.0 (arm64) and Linux 6.18.5 (arm64) on 2026-08-23.

- `pollmask.c` — the per-phase level for every socket phase the readiness
  model knows, the non-socket descriptor kinds, and the argument screens.
- `pollmulti.c` — `*triggered` over a mixed array, the "already ready returns
  immediately at any timeout" claim, `events = 0` on a socket with `HUP`, the
  Darwin post-refusal latch, and large `nfds`.
- `pollgaps.c` — the rows the first two got wrong or missed: a *regular* file
  (not just `/dev/null` and a directory), a connect genuinely still in flight,
  and the AF_UNIX socket kinds.
- `pollimmediate.c` — whether a *positive* timeout still returns at once when
  the only thing making the entry ready is an output-only condition: an
  unrequested `HUP`, or `NVAL`. `pollmulti.c` had measured that only for a
  *requested* `POLLOUT`, and PawPrint's ready predicate asserts these two
  cases.
- `pollnfds.c` — the `nfds` bound. It is `RLIMIT_NOFILE` on Linux; Darwin
  refuses at 65536 despite a far larger rlimit. PawPrint models no descriptor
  limit at all (`FileDescriptorRegistry.fs`: `RLIMIT_NOFILE` is not in the
  interop surface), so it answers as though the limit were unbounded, which is
  the same envelope the rest of the registry already states.

Two rows in these probes are known-contaminated and are not quoted in the
plan; each says so in a comment where it is used:

- `pollmask.c` rows 10/11 ("connect in flight"): the loopback connect had
  already completed, so both rows measure the completed state. `pollgaps.c`
  row 4 replaces it.
- `pollgaps.c` row 5 ("in flight, 300ms later"): something in the sandbox's
  NAT answers for `192.0.2.1`, so this reports `OUT` on both kernels rather
  than a still-retrying SYN. Row 4, taken immediately, is the clean reading —
  though strictly that is the same race with a slower responder, so if anyone
  ever needs to lean on it, use a firewalled loopback port instead. Nothing in
  this slice consumes the row: PawPrint has no in-flight connect phase.
- `pollimmediate.c` row 8 ("events=0x7FC0"): this measures **libc**, not the
  PAL. `0x7FC0` is not undefined to Linux — it contains `POLLWRNORM` (0x100)
  among others, which is why Linux reports `0x110` rather than `HUP` alone.
  At the `SystemNative_Poll` surface the question does not arise:
  `Common_ConvertPollEventsPalToPlatform` has exactly six rows, so every bit
  outside them is dropped before the kernel sees the request. That is a
  source fact, and the row is kept only to show libc behaves differently.
