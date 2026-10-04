# WoofWare.PawPrint.Analysis

Questions about a .NET method answered without running it.

The first is which exceptions can escape it. `EscapeAnalysis.escapes` reads a method's IL and that
of everything it calls, following calls into whatever assemblies they live in, and answers with the
exception types it can name and whether anything it could not see through (a virtual call, a native
method) may add more:

* an exception thrown is named by what the IL says its operand is, over every path to the `throw`:
  exactly where a path constructs it, and as "this type or a subtype" where a path holds it in a
  value of that static type (an argument, a local, a field, a call's result, a cast);
* a `callvirt` of a method a derived class may override, on an object the IL says is exactly of one
  class (one a `newobj` made), or of a sealed class or a boxed value type, runs that class's
  implementation, found as for a `constrained.` call below; one on an object that may be of a class
  others derive from is opaque. An object a call returns is of the classes the callee's `ret`s
  return, in the instance the call reaches, which may be narrower than its declared return type
  (`Encoding.UTF8` returns a field of a sealed class); a return that depends on itself is only its
  declared type;
* `throw null` raises the `NullReferenceException` that throwing a null does, and nothing else;
* a `rethrow` re-raises what its `catch` clause caught: what the clause's protected block raises and
  no clause tried before it stops, of the clause's type. Something the analysis cannot name, caught
  by a clause for one exception class, is re-raised as "that class or a subclass", where the clause
  sees only exceptions: in an assembly that wraps non-exception throws, and a class that
  `RuntimeWrappedException` does not derive from. A `filter`'s handler re-raises whatever reached its
  filter;
* the faults the runtime raises by itself come from `WoofWare.PawPrint.Semantics`' `OpcodeFaults`,
  the same table the PawPrint interpreter raises them through;
* an `[Intrinsic]` is read as what CoreCLR runs for it (`IntrinsicBody`): the IL its VM substitutes
  (`VmSubstitution`), or its own IL with its call to itself performed as the JIT expands it
  (`IntrinsicBody.expandSelfCall`, which the interpreter follows too), for the JIT target and the
  CPU (`HardwareIntrinsicsProfile`) the analysis is created for. A capability query raises nothing;
  a hardware instruction the CPU lacks raises `PlatformNotSupportedException`, and one it has
  raises what the JIT's tables say (`HardwareInstruction`); one of the runtime's primitives raises
  what its contract (`IntrinsicPrimitive`) says it can;
* a method CoreCLR implements in native code is opaque, unless `NativeMethod` describes it (so far,
  the maths functions `Math` and `MathF` call, CoreLib's P/Invokes into the framework's own
  native libraries, and the FCalls and QCalls of its contract table), when it raises what that
  contract says. A `newobj` of a constructor of
  `System.String`, which is native, calls the `String.Ctor` CoreCLR runs in its place
  (`StringConstructor`);
* code a capability query rules out on that CPU is left out. Where a call to an `IsSupported` or
  `IsHardwareAccelerated` that answers a constant (`IntrinsicBody.constantResult`) is branched on
  straight away (`brtrue` or `brfalse`, which nothing else reaches), only the way the answer goes is
  followed (`ControlFlow.mayExecute`); every other branch is followed both ways. A handler counts
  only if something in its protected block does. Tokens in code left out are still bound, below,
  which reports more than the JIT does: it folds such a branch, even under MinOpts, and never binds
  what only the other way reaches;
* a `constrained.` call on a value type or a sealed class runs that type's own implementation,
  and one of a static virtual method runs the implementation the type supplies, whatever derives
  from it; the analysis finds either with `WoofWare.PawPrint.TypeSystem`'s `ConcreteVirtualDispatch`, the
  dispatch the interpreter runs, or raises `AmbiguousImplementationException` where two default
  interface bodies are equally specific. A generic method's IL is read once, and its summary computed for
  each closed instantiation a call reaches, which decides what a `constrained.` call on one of its
  type variables runs. Asked about by itself, a generic definition stands for every instantiation,
  so such a call is opaque in it, as is one of an instance method on a class that may be derived
  from, whose instance may be of a class that overrides the method, one whose default bodies conflict through a variant
  interface, one on a type whose base types or interfaces, or the signature of a method of any of
  them, name a type that is not there, and one naming a method whose own body has a local of such a
  type. An instantiation nested more than eight deep is analysed as its definition, so that a
  method calling itself at ever deeper instantiations reaches finitely many;
* a `catch` absorbs what derives from its type, decided on the real base chains of types in any
  assembly, which `WoofWare.PawPrint.Loader` resolves exactly as the interpreter does;
* an object thrown that is not an exception is named as itself; a `catch` sees it as a
  `RuntimeWrappedException` if its method's assembly wraps such throws (C# and Visual Basic
  assemblies do, F# ones do not), and as itself if not;
* a `TypeInitializationException` from a type initializer is left out wherever the type a call
  touches has none;
* every token a body names, the type of every local and `catch` clause it declares, and the
  signature of every method it calls, directly or through `calli`, is bound against the assemblies actually loaded, and a member
  or type that is not there contributes the `MissingMethodException`, `MissingFieldException` or
  `TypeLoadException` the JIT would throw. A member of one of the method's type variables
  (`!!0::Value`) exists or not depending on the instantiation, so binding it is "unknown";
* binding a token into another assembly that has a module initializer runs it, which may throw
  `TypeInitializationException`, and so does a `constrained.` call that dispatch lands in another
  assembly;
* a synchronized method takes a monitor around its body, whose wait may throw
  `ThreadInterruptedException`, whose release throws `SynchronizationLockException` if the body
  has already released it, and which, for an instance method `call`ed on null, throws
  `ArgumentNullException`.

  All these happen outside the body, so the body's own handlers do not catch them.

The answer is an over-approximation: whatever a run can let escape is in it, named or covered by
"unknown". Resource exhaustion is in it too, so almost every method can escape
`StackOverflowException`.

One callback is assumed to behave as documented rather than analysed. Casting an object to an
interface, or storing it in an array (whose element type may be an interface), calls
`IDynamicInterfaceCastable.IsInterfaceImplemented` if the object's class implements that interface.
The analysis assumes that throws nothing but the `InvalidCastException` its documentation asks for;
an implementation that throws anything else can let that escape unreported. The interface's other
callback, `GetInterfaceImplementation`, is reached only by an interface call or an interface `ldvirtftn` on
an object whose class does not implement the interface. The analysis counts an interface
`ldvirtftn` as opaque, and resolves an interface call only through a `constrained.` type that
implements the interface itself, or on an object the IL says is of a class that does.

The analysis also assumes the IL is well typed: a value held where the IL spells a type (an
argument, a local, a field, a call's result) is of that type, as the JIT assumes when it
devirtualises. Code that breaks that with `Unsafe.As`, or by reinterpreting memory, is outside what
the analysis answers for.

It assumes too that the native libraries the framework ships with its CoreLib
(`libSystem.Native` and `libSystem.Globalization.Native`) are present and export what its P/Invokes
import, so that calling one of their functions raises nothing: native code raises no exception the
caller can catch, and an exception thrown by a managed function it calls back ends the process.
Were one of those libraries missing, the runtime would run the program's `ResolvingUnmanagedDll`
handlers, which could throw anything.

That holds for a set of assemblies that agree with each other. When one has changed since another
was compiled against it, a missing member or type is reported as above, and says that the set
needs rebuilding (or, for a package, a different version). Other ways such a change can break a
caller are not checked, and are not reported: a member made inaccessible to it
(`FieldAccessException`, `MethodAccessException`), a generic instantiation it spells that a
constraint added since now rejects, and a type it names whose own base type, interfaces or fields
are no longer there (`TypeLoadException`).

An assembly that is not there at all, where a type was missing from one that is, is reported as
`FileNotFoundException` wherever a token, a local or a `catch` clause names a type in it, and as
"unknown": before raising that, the runtime runs the program's `AssemblyLoadContext.Resolving` and
`AppDomain.AssemblyResolve` handlers, which may throw (the runtime then raises a `FileLoadException`
wrapping what they threw), or load an assembly in its place whose code then runs.

This package sees `WoofWare.PawPrint.Domain`, `WoofWare.PawPrint.Loader`,
`WoofWare.PawPrint.TypeSystem` and `WoofWare.PawPrint.Semantics`, and never the interpreter.
