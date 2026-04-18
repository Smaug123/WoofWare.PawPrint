# Implementing a new IL instruction

Follow these steps when adding support for a new IL opcode to the PawPrint interpreter.

## Workflow

1. Add the instruction to `IlOp.fs`
2. Implement execution logic in `AbstractMachine.fs`
3. Add a test case in `sourcesPure/` or `sourcesImpure/` (C# file) that exercises the instruction
4. Pure cases in `sourcesPure/` are auto-discovered; impure cases in `sourcesImpure/` must also be added to `TestImpureCases.fs`. The `.fsproj` already includes both directories as wildcard `EmbeddedResource`s.
5. Run tests to verify implementation, using `nix develop -c dotnet ...`
