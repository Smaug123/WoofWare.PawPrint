# Fuzzing over the scheduler

PawPrint is a single-threaded deterministic IL emulator.
It advances one thread at a time, one IL instruction at a time.

The default thread scheduler (passing `None` as the optional seed arg to `Program.run`) is a simple round-robin scheduler.
Alternatively, by passing a seed `Some 3uL` to `Program.run`, you get a [Probabilistic Concurrency Testing](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/asplos277-pct.pdf) (PCT) scheduler.
We'll discuss PCT only, because that's how you fuzz.

## How to do it

See [How to run a program](./run-a-program.md) for instructions on running a program.
The important thing you need to change to perform fuzzing is simply to supply `Some myFavouriteNumber` as the seed argument to `Program.run`; for example, `Some 0uL`.

By calling `Program.run` many times with different seeds, you can observe different thread execution orders.
Replaying a given seed is always deterministic.

PawPrint's own tests do this, in [TestRaces.fs](../../WoofWare.PawPrint.Test/TestRaces.fs) and [TestConcurrencyBugs.fs](../../WoofWare.PawPrint.Test/TestConcurrencyBugs.fs).

## Effectiveness

Weirdly effective!

When I first tested this, over the seeds 0-49, [ReadWriteRace.cs](../../WoofWare.PawPrint.Test/sources/ReadWriteRace.cs) observed an exit code 0 with count 32, and exit code 1 with count 18.
That is much closer than I was expecting to 50/50 at exhibiting the two possible outcomes of that program.

## Description of the scheduling algorithm

The PCT scheduling algorithm as implemented in PawPrint is roughly as follows.

Every running thread has a "priority", drawn randomly from `[0,1)`.

Some IL ops are known to be uninteresting (like `Pop`, which has no observable side-effects on any other thread); some are extremely interesting (like `Stfld`, which mutates program-global state and is therefore very visible to other threads).

Repeatedly:

* Find the highest-priority thread, and observe its pending IL op.
* With high probability if the op is interesting, or low probability if the op is not interesting, do the following:
  * assign the current thread a new priority, drawn at random from `[0, 1)`.
* Select the highest-priority thread and execute its next IL op.

The overall effect is that if we're currently choosing to execute a thread, and its operation is uninteresting, we usually won't reassign its priority and so we will continue to execute it, until it hits an interesting op.
On the other hand, if its operation is interesting, then we're more likely to reassign the current thread's priority, and that gives it a higher chance of being demoted in priority.