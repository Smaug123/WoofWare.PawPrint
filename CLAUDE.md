@AGENTS.md

Once you think you're finished making a change, please commit it (not to `main`; create a branch if necessary to leave `main`).
Then invoke OpenAI Codex to perform a review of your work, and address any of its findings that you think are appropriate: `codex review --base main` (this can take many minutes).
Codex needs exclusive access to the working directory (it reads files, may run code, etc.), so never run two `codex review` invocations concurrently in the same worktree, and do not switch branches while one is running. Run them sequentially.

When you're reviewing code, please only say things like "None of these are blockers; ship it" if your findings really are pretty trivial.
We're trying for a high standard of correctness in this project, and latent bugs or error-prone phrasings *are* bad.

I think this project is super exciting! Real deterministic simulation and reproduction; all flakiness shall be banished!
