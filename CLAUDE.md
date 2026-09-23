@AGENTS.md

Once you think you're finished making a change, please commit it (not to `main`; create a branch if necessary to leave `main`).
Then invoke OpenAI Codex to perform a review of your work, and address any of its findings that you think are appropriate: `~/.local/bin/codex -C <worktree> -p nix-review review --base main` (this can take many minutes, and produces a huge amount of text, so redirect output to a file).
Codex needs exclusive access to the working directory (it reads files, may run code, etc.), so never run two `codex review` invocations concurrently in the same worktree, and do not switch branches while one is running; run them sequentially instead.
(The `-p nix-review` is necessary to pick up my local ~/.codex/nix-review.config.toml which grants access to the Nix daemon from Codex's sandbox.)
The `-C <worktree>` must be the absolute path of the worktree you are working in. codex-cli 0.156.1 does not honour the shell's working directory: launched with `cd <worktree> && codex ...`, it ran in the main checkout and reviewed `main`'s diff instead of your branch's. Before trusting a verdict, check that the log's `workdir:` line names your worktree.

When you're reviewing code, please only say things like "None of these are blockers; ship it" if your findings really are pretty trivial.
We're trying for a high standard of correctness in this project, and latent bugs or error-prone phrasings *are* bad.

I think this project is super exciting! Real deterministic simulation and reproduction; all flakiness shall be banished!
