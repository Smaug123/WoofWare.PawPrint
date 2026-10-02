#!/usr/bin/env python3
# Reduces section B of kqueue-kevent.c's output to the rule it measured, and
# fails if any line disagrees with that rule.
#
#   python3 kqueue-kevent-analyse.py kqueue-kevent.darwin-27.0.txt
#
# The rule, in the order kevent(2) decides it:
#   1. an unreadable timeout is EFAULT, and a timeout with tv_sec < 0,
#      tv_sec > INT32_MAX, tv_nsec < 0 or tv_nsec > 1000000000 is EINVAL;
#   2. a descriptor that is not open, or names anything but a kqueue, is EBADF;
#   3. nchanges > 0 with a changelist whose first entry cannot be read is
#      EFAULT (nchanges <= 0 reads no change at all);
#   4. nevents <= 0 returns 0 at once, whatever the timeout;
#   5. a kqueue with nothing to report returns 0 at once for {0,0}, returns 0
#      at a positive timeout, and sleeps for ever for a NULL one. The eventlist
#      is never looked at, so NULL and unreadable ones answer the same.
# WoofWare.PosixKernel.Test's TestKqueue holds the library to every line.
import sys

INVALID = {'{0,-1}', '{-1,0}', '{-1,1}', '{INT64_MAX,0}', '{INT32_MAX+1,0}', '{0,1e9+1}'}
SLEEPS_FOR_EVER = {'NULL', '{0,1e9}', '{INT32_MAX,0}'}


def predict(changes, fd, nevents, events, timeout):
    if timeout == 'unreadable':
        return ('-1', 'EFAULT', '0')
    if timeout in INVALID:
        return ('-1', 'EINVAL', '0')
    if fd not in ('kqueue', 'kqueue-dup'):
        return ('-1', 'EBADF', '0')
    if changes == '1/unreadable':
        return ('-1', 'EFAULT', '0')
    if int(nevents) <= 0:
        return ('0', '-', '0')
    if timeout == '{0,0}':
        return ('0', '-', '0')
    if timeout == '{0,20ms}':
        return ('0', '-', '1')
    # Cut short by the probe's 40 ms timer.
    assert timeout in SLEEPS_FOR_EVER, timeout
    return ('-1', 'EINTR', '1')


rows = [line.rstrip('\n').split('\t') for line in open(sys.argv[1]) if line.startswith('B\t')]
bad = 0
for row in rows:
    got = (row[6].replace('rv=', ''), row[7], row[8].replace('slept=', ''))
    want = predict(*row[1:6])
    if got != want:
        bad += 1
        print('disagrees:', row, 'predicted', want)
print(f'{len(rows)} lines, {bad} disagreeing')
sys.exit(1 if bad else 0)
