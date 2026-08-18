# Probing this codebase before implementing

A "probe" is a throwaway measurement — a temporary `failwith`, a minimal repro compiled and run against real .NET or PawPrint — used to establish ground truth before writing a fix or a comment about behaviour. Three ways a probe can look successful while measuring the wrong thing.

## Re-entrant branches look like straight-line code

A native handler that returns `SuspendedForClassInit` / `BlockedOnClassInit` / `PushedManagedCallee` gets re-entered later, so *any* branch that can reach one of those results is re-entrant — including the "first entry" branch, which re-runs from the top with the eval stack empty. This is invisible at the point you read the code: the arm looks like a normal early return. Three successive comments in `NativeReflectionInvocation.fs` claiming when `resolveTarget` ran were each accurate about the lines in front of them and wrong about control flow, because guest code (a `.cctor`) runs between the two reads.

**How to apply:** before writing any claim about ordering, "runs once", or that a re-entry point is unreachable, temporarily replace the suspicious arm with `failwith "PROBE: <arm> really happens here"` and run the test you think exercises it. Thirty seconds, and it settles both whether the path is live and whether the claim is true.

## A newer measurement doesn't supersede an older one about the same mechanism

Probing dynamic `ldstr` interning, probe 1 showed the emitted object came back from `ReferenceEquals`. Probe 2 then showed a previously-interned literal wins over the emitted object, and the natural move was to rewrite the model around "interns by value" — true but incomplete, since the miss path interns *the guest's own object*. Probe 1's row was the disproof, and it had already been run.

**How to apply:** when a new measurement changes your conclusion, re-read every earlier row and check the new model predicts each one — write the rows into a table so the check is forced. When a conclusion has two candidate poles, suspect the real answer is a rule that picks between them by state, and design the probe to hit each side deliberately.

## A probe can address the wrong entity, or never read it

Measuring what real .NET does when a dynamic method's `DynamicScope` slot is rewritten, `ldstr "abcd"; callvirt String.get_Length; ret` was used with `tokens[Count - 1]` rewritten — but `callvirt` appends a *method* entry after the string, so "the last slot" was the method, not the string, and the recorded exception types were wrong. Fixing the entity, the probe then failed the *other* way: `ldstr "abcd"; pop` never materialises the literal on real .NET, so every rewrite came back as a pass regardless of whether it worked. Only `ldstr "abcd"; ret` — where the value is actually used — measured the thing.

A probe has two failure modes that look identical to success: it can address the wrong entity (the index/handle/slot computed is not the one named), or it can address the right entity but never cause it to be read. Both yield clean, plausible numbers, and neither is caught by a control that only checks "the untouched case still works" — that control passes in both wrong versions.

**How to apply:**
1. Print the fixture itself, not just the result — dumping a scope's contents by index would have shown the mismatch immediately.
2. Confirm the perturbation actually reaches the code under test: at least one perturbation must *change the answer*. If every mutation gives the same result, the target is dead.
3. Name the index/entity explicitly rather than by position (`Count - 1` is a guess about layout).

A probe is a test, and an unfalsifiable one is worth nothing.
