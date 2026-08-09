# Fuzzing over the scheduler

PawPrint is a single-threaded deterministic IL emulator.
It advances one thread at a time, one IL instruction at a time.

The default thread scheduler (leaving `HostConfig.PctSeed` at its default of `None`) is a simple round-robin scheduler.
Alternatively, by setting `PctSeed = Some 3uL` on the `HostConfig` you hand to `Program.run`, you get a [Probabilistic Concurrency Testing](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf) (PCT) scheduler.
We'll discuss PCT only, because that's how you fuzz.

## How to do it

See [How to run a program](./run-a-program.md) for instructions on running a program.
The important thing you need to change to perform fuzzing is simply to set `PctSeed = Some myFavouriteNumber` on the `HostConfig`; for example, `Some 0uL`.

By calling `Program.run` many times with different seeds, you can observe different thread execution orders.
Replaying a given seed is always deterministic.

PawPrint's own tests do this, in [TestRaces.fs](../../WoofWare.PawPrint.Test/TestRaces.fs) and [TestConcurrencyBugs.fs](../../WoofWare.PawPrint.Test/TestConcurrencyBugs.fs).

## Effectiveness

Weirdly effective!

When I first tested this, over the seeds 0-49, [ReadWriteRace.cs](../../WoofWare.PawPrint.Test/sources/ReadWriteRace.cs) observed an exit code 0 with count 32, and exit code 1 with count 18.
That is much closer than I was expecting to 50/50 at exhibiting the two possible outcomes of that program.

## Description of the scheduling algorithm

The PCT scheduling algorithm as implemented in PawPrint is roughly as follows.

Every thread has a "priority". A thread's initial priority is drawn randomly from `[0,1)`, the first time the scheduler sees it runnable.

Repeatedly:

* Find the highest-priority runnable thread.
* With probability 1%, *demote* it: give it a priority strictly below that of every other thread the scheduler has ever demoted, and hence below every thread that has not been demoted more recently.
* Select the highest-priority runnable thread and execute its next IL op.

So a thread runs until a demotion draw succeeds, at which point the machine goes to somebody else; a run of length *n* contains about *n*/100 context switches.
Demotion is to the bottom rather than to a fresh random priority, which matters more than it sounds: a uniform redraw lands back above the other *n-1* threads about `1/n` of the time, so the thread that was just demoted often immediately wins again and residency becomes a heavy-tailed random walk instead of a rotation.

Two deliberate departures from the paper are worth knowing about if you are reasoning about coverage.

Burckhardt et al. sample a fixed number of priority-change points from a known total step count, which is what buys PCT its probabilistic bug-finding guarantee.
PawPrint's runs are open-ended, so there is no such count to sample from, and demotion is a per-step coin flip instead. You get the algorithm's behaviour without its bound.

Because the demotion floor only ever descends, the priority *order* among threads that are permanently runnable is a rotation fixed by the initial draw.
A run with *n* such threads therefore contains exactly *n* distinct hand-off pairs: within a single seed you will not see both "A is preempted by B" and "B is preempted by A".
All such pairs remain reachable across different seeds, and in real guests blocking, waking, and thread creation all perturb the cycle — but if you are fuzzing, vary the seed rather than lengthening one run.

Note also what the scheduler no longer does.
It used to inspect each thread's pending IL op and scale the demotion probability by how "interesting" that op looked, on the theory that switching at a `Stfld` explores more than switching at a `Pop`.
That made expected residency inversely proportional to how interesting a thread's instructions were — precisely backwards — and a thread executing nothing but branches (`while (true) { }` compiles to a single `br`) could never be demoted at all, so it kept the machine indefinitely.
Choosing the *site* of a switch also turns out to buy nothing: preempting at an op no other thread can observe gives the same interleaving of visible operations as preempting at the next visible op instead.