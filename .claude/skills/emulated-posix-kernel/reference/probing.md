# Probing both platforms

Establish a kernel fact by running a probe on each platform rather than by
reading a header. Two environment facts make a probe report the wrong answer
convincingly.

## Set-ID bits need a group you belong to

A non-root `chmod` refuses to set `S_ISGID` when the file's group is not one the
caller belongs to. Under BSD semantics a new file inherits its *directory's*
group, and `/tmp` on macOS is group `wheel` — so `chmod 2755 /tmp/x` fails for an
ordinary user, and every set-ID row of a probe reads as "unsettable on this
platform" when it is nothing of the kind. `chown(path, -1, getegid())` first, and
`02755`, `02644`, `06755` all set fine at uid 501.

Measuring as root is worse than useless here: root preserves every set-ID bit on
both platforms, so it hides the divergence rather than revealing it.

Any probe that sets special mode bits must therefore (a) `chgrp` to the caller's
own group first, and (b) **read the mode back and report what actually stuck**,
so a silently-refused `chmod` can never be misread as a preserved bit. A host
oracle should drop such a row rather than compare it.

## The uname release is not the running kernel

`SimulatedUnixPlatform.linuxX64` names a release string because that is what a
guest reads from `uname`. Nothing ties it to the kernel a test actually runs on,
so source read at the preset's version is not evidence about the host: x86
replaced `valid_user_address`'s sign-bit test with a comparison against
`USER_PTR_MAX` between 6.9 and 6.12, and `ubuntu-latest` runs newer than the
preset's string.

The same test caught that `container --arch amd64` presents an arm64 kernel
behind an `x86_64` uname. Check `/proc/self/maps`, not `uname -m`.

## Write the measuring test first

For any kernel-behaviour constant, write the measure-the-host test before the
implementation and let it name the value. Use source only to learn the rule's
*shape* — is this a pointer test or a range test? — which is far more stable
across versions than the constant is.
