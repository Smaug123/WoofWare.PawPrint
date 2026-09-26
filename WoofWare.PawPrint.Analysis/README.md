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
* an object thrown that is not an exception is named as itself; a `catch` sees it as a
  `RuntimeWrappedException` if its method's assembly wraps such throws (C# and Visual Basic
  assemblies do, F# ones do not), and as itself if not;
* a `TypeInitializationException` from a type initializer is left out wherever the type a call
  touches has none;
* every token a body names, the type of every local and `catch` clause it declares, and the
  signature of every method it calls, is bound against the assemblies actually loaded, and a member
  or type that is not there contributes the `MissingMethodException`, `MissingFieldException` or
  `TypeLoadException` the JIT would throw;
* binding a token into another assembly that has a module initializer runs it, which may throw
  `TypeInitializationException`;
* a synchronized method takes a monitor on entry, whose wait may throw
  `ThreadInterruptedException`.

  All these happen before the body runs, so the body's own handlers do not catch them.

The answer is an over-approximation: whatever a run can let escape is in it, named or covered by
"unknown". Resource exhaustion is in it too, so almost every method can escape
`StackOverflowException`.

One callback is assumed to behave as documented rather than analysed. Casting an object to an
interface, or storing it in an array (whose element type may be an interface), calls
`IDynamicInterfaceCastable.IsInterfaceImplemented` if the object's class implements that interface.
The analysis assumes that throws nothing but the `InvalidCastException` its documentation asks for;
an implementation that throws anything else can let that escape unreported.

That holds for a set of assemblies that agree with each other. When one has changed since another
was compiled against it, a missing member or type is reported as above, and says that the set
needs rebuilding (or, for a package, a different version). Other ways such a change can break a
caller are not checked, and are not reported: a member made inaccessible to it
(`FieldAccessException`, `MethodAccessException`), a generic instantiation it spells that a
constraint added since now rejects, and a type it names whose own base type, interfaces or fields
are no longer there (`TypeLoadException`).

This package sees `WoofWare.PawPrint.Domain`, `WoofWare.PawPrint.Loader` and
`WoofWare.PawPrint.Semantics`, and never the interpreter.
