# WoofWare.PawPrint.Analysis

Questions about a .NET method answered without running it.

The first is which exceptions can escape it. `EscapeAnalysis.escapes` reads a method's IL and that
of everything it calls, following calls into whatever assemblies they live in, and answers with the
exception types it can name and whether anything it could not see through (a virtual call, a native
method, a `rethrow`) may add more:

* an exception a method constructs and throws is named exactly; one returned by a helper and thrown
  is named as "this type or a subtype";
* the faults the runtime raises by itself come from `WoofWare.PawPrint.Semantics`' `OpcodeFaults`,
  the same table the PawPrint interpreter raises them through;
* a `catch` absorbs what derives from its type, decided on the real base chains of types in any
  assembly, which `WoofWare.PawPrint.Loader` resolves exactly as the interpreter does;
* a `TypeInitializationException` is left out wherever the type a call touches has no initializer.

The answer is an over-approximation: whatever a run can let escape is in it, named or covered by
"unknown". Resource exhaustion is in it too, so almost every method can escape
`StackOverflowException`.

This package sees `WoofWare.PawPrint.Domain`, `WoofWare.PawPrint.Loader` and
`WoofWare.PawPrint.Semantics`, and never the interpreter.
