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

Two rows in these probes are known-contaminated and are not quoted in the
plan; each says so in a comment where it is used:

- `pollmask.c` rows 10/11 ("connect in flight"): the loopback connect had
  already completed, so both rows measure the completed state. `pollgaps.c`
  row 4 replaces it.
- `pollgaps.c` row 5 ("in flight, 300ms later"): something in the sandbox's
  NAT answers for `192.0.2.1`, so this reports `OUT` on both kernels rather
  than a still-retrying SYN. Row 4, taken immediately, is the clean reading.
