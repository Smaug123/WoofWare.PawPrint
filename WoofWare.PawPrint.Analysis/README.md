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
  declared type. An argument is of the classes the call reaching the method passes it, where the
  caller decides them, and is null or not as the caller decides that: a method's summary is
  computed for each combination of objects its callers pass as the arguments its body uses as a
  receiver, returns, passes on or tests, so a helper called with a `new Dog()` runs `Dog`'s
  overrides there, and `ArgumentNullException.ThrowIfNull` called with a `new object()` throws
  nothing. An argument of an integer type (`bool`, `char`, or an integer of up to 64 bits) is the
  constants the caller passes it, where the caller decides them, each converted to the parameter's
  type as the runtime converts it (a `bool` holds the low byte of an `int` passed for it), so
  `ObjectDisposedException.ThrowIf` called with `false` throws nothing. A `ref` argument points to
  the value the caller's local held when the call was made, where the caller decides it: a local of
  an integer type whose address, since any path took it, has met nothing but loads, branches,
  arithmetic and stores to the caller's own arguments and locals, so that nothing else can hold
  it. The method knows that value for a load through
  the argument (of the local's width or narrower: CoreCLR's CPUs are little-endian) made before
  it stores to anything but its own arguments and locals or runs other code, so `Monitor.Enter`,
  passed the `lockTaken` a `lock` statement sets to `false`, throws no `ArgumentException`. A
  `lock` inside a loop passes a local whose address the previous iteration passed to `Enter`,
  and stays imprecise. An object a call returns is
  null or not as the callee's `ret`s return it. Asked about by itself, a method may be passed any
  value its parameter types admit, null among them, and so may one whose body stores to the
  argument or takes its address;
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
  native libraries, and the FCalls and QCalls of its contract table), and the caller allows each
  assumption its contract holds under (`NativeContract.Assumes`), when it raises what that
  contract says, and what the parameterless constructor of each of those exceptions raises, which
  CoreCLR runs to make it, and what each CoreLib method the contract says it calls raises, passed
  arguments the analysis cannot name. A `newobj` of a constructor of
  `System.String`, which is native, calls the `String.Ctor` CoreCLR runs in its place
  (`StringConstructor`);
* code that a capability query rules out on that CPU, or that a test of whether an object is null
  or an integer is zero rules out, is left out. Where a call to an `IsSupported` or `IsHardwareAccelerated` that answers
  a constant (`IntrinsicBody.constantResult`; another architecture's query answers false) is branched on straight away (`brtrue` or `brfalse`,
  which nothing else reaches), only the way the answer goes is followed (`ControlFlow.mayExecute`),
  and so it is where the value branched on straight away is an object known to be null or not: one
  `newobj` or `ldstr` made, `ldnull`, or an argument whose callers all pass an object or all pass
  null; or an integer known to be zero or not: a constant an `ldc` pushed, or what a conversion
  that does not check for overflow (`conv.i1` to `conv.u8`, `conv.i`, `conv.u`) makes of one, an argument whose
  callers all pass zero or all pass something else, or a load through a `ref` argument whose
  callers' locals all hold zero or all hold something else; or a call of
  `RuntimeHelpers.IsReferenceOrContainsReferences<T>()`, where the instance says what `T` is, which
  is a constant however the JIT compiles the method (`GcReferences`). Every other branch is followed both ways. A handler counts only if something in its
  protected block does. Tokens in code left out are still bound, below, which for a capability
  query reports more than the JIT does: it folds such a branch, even under MinOpts, and never
  binds what only the other way reaches;
* where the caller allows `Assumption.JitOptimises`, `typeof(X).IsValueType` is the constant the JIT
  compiles it to: `ldtoken X`, then calls of `Type.GetTypeFromHandle` and `Type.get_IsValueType`
  that no branch lands between, call neither method, and a
  `brtrue` or `brfalse` straight after follows only the way the answer goes, where the instance
  says what `X` is. So too `typeof(X) == typeof(Y)`, or `!=`, which the JIT compiles to a
  comparison of the two types' handles: `ldtoken X`, a call of `GetTypeFromHandle`, `ldtoken Y`,
  another, then a call of `Type.op_Equality` or `op_Inequality`, which no branch lands between,
  call none of those methods, and a branch straight after follows only the way the answer goes,
  where the instance says what both types are. So too any other `t == typeof(X)`, or `!=`, which
  the JIT compiles to a comparison of `t` with X's `RuntimeType` by reference: whatever pushes `t`,
  `ldtoken X`, a call of `GetTypeFromHandle`, then one of `op_Equality` or `op_Inequality`, which
  no branch lands between, call neither method, and a branch on it is followed both ways. What
  pushes `t` runs as written: `o.GetType()` still calls `Object.GetType`, which the JIT expands
  only past tier 0;
* `ldtoken` of a type gives its `RuntimeType`, which the JIT embeds as it compiles where it knows
  the type exactly. In generic code that CoreCLR shares between instantiations
  (`CodeSharing`), a type that names a type variable whose argument makes it share the code is
  one the code looks up at run time, and there the JIT calls
  `RuntimeTypeHandle.GetRuntimeTypeFromHandle`, which raises only `OutOfMemoryException` and
  `StackOverflowException` there (see below). Where the JIT compares two types' handles, above, it makes
  neither `RuntimeType`; where it compares `t` with `typeof(X)` by reference, it makes X's. One
  case is not modelled: the runtime makes the `RuntimeType` at run time for a type of an assembly
  loaded into a collectible context, too;
* `ldtoken` of a method or a field gives a handle holding a stub object, which the JIT makes by
  calling `RuntimeMethodInfoStub.FromPtr` or `RuntimeFieldInfoStub.FromPtr`, in shared code as
  elsewhere, and the analysis follows that call. Where the caller allows `JitOptimises`, an
  `ldtoken` of a field then a call of `RuntimeHelpers.CreateSpan<T>`, which no branch lands on, is
  a span over the field's data, calling neither method, where the field is a `FieldDef` of a type
  that is not generic, it has data at least one `T` long, and `T` is a primitive type or an enum.
  The JIT expands `RuntimeHelpers.InitializeArray` of a field's handle too, but only where it
  optimises fully, which a method it compiles first does not, so there the analysis follows both
  calls;
* `isinst`, `castclass`, and `unbox.any` of a reference type call the `CastHelpers` method the JIT
  picks for the type (`getCastingHelperStatic`): an `IsInstanceOf...` one for `isinst` and a
  `ChkCast...` one otherwise, of `Class` for a class or a value type, `Interface` for an interface,
  and `Any` for the rest (an array, a type with variant type parameters, a type variable in shared
  code, a `Nullable<T>` shared over its argument), which the analysis follows. Every one but
  `IsInstanceOfClass` falls back to the runtime where the cast cache has no answer, which holds
  under `NamedTypesLoad` and `DynamicCastsAsDocumented`. Where only an instance decides the method
  (a cast to a type variable, or to a `Nullable<T>` over one), a generic definition calls each
  some instance does, and an instance the one its code does. The JIT decides a cast without a call
  only where it optimises fully, or where it folds a `box` the cast tests, which the analysis does
  not follow: it follows the call there too. It takes the runtime to be a Unix one, wherever the
  operating system decides what CoreCLR does (Windows is not modelled): one that casts to no type
  by type equivalence;
* `unbox`, and `unbox.any` of a value type, call `CastHelpers.Unbox`, or `Unbox_Nullable` for a
  `Nullable<T>` (`getUnBoxHelper`); `box` of a struct calls `Box`, and of a `Nullable<T>`,
  `Box_Nullable`, while the JIT boxes a primitive type or an enum inline and a reference type not at
  all (`impImportAndPushBox`); `ldelema` of a reference type with no `readonly.` prefix calls
  `LdelemaRef`; and `stelem.ref`, and `stelem` of a reference type, call `StelemRef`, even to store
  null, which falls back to the runtime as the casts do. The analysis follows each, and, where a
  type variable decides the method, a generic definition calls each some instance does, and an
  instance the one its code does. The JIT boxes without the call where it optimises fully, or where
  it folds the box, and stores without it where it optimises and can show the store needs no type
  check (`gtCanSkipCovariantStoreCheck`), which the analysis does not follow. A `constrained.` call
  on a value type boxes its receiver the same way where the method it runs is no method the type
  declares: one it inherits from `Object`, `ValueType` or `Enum`, a default interface body, or one
  that is not virtual, such as `Object.GetType` (`CORINFO_BOX_THIS`, `getCallInfo`). The analysis
  follows that box too, as an instance's dispatch decides it. The JIT refuses to box a ref struct
  there (`InvalidProgramException`), which the analysis does not follow: such a call is opaque, as
  is one on a type variable that allows a ref struct in a definition asked about by itself;
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
  them, name a type that is not there, one naming a method whose own body has a local of such a
  type, and one naming a method whose signature names a type that does not load. An instantiation nested more than eight deep is analysed as its definition, so that a
  method calling itself at ever deeper instantiations reaches finitely many;
* a `catch` absorbs what derives from its type, decided on the real base chains of types in any
  assembly, which `WoofWare.PawPrint.Loader` resolves exactly as the interpreter does;
* an object thrown that is not an exception is named as itself; a `catch` sees it as a
  `RuntimeWrappedException` if its method's assembly wraps such throws (C# and Visual Basic
  assemblies do, F# ones do not), and as itself if not. Either way the runtime wraps it where it is
  thrown, allocating the `RuntimeWrappedException` and running its `(object)` constructor, and
  throws in the wrapper's place what either raises, so the answer has that too;
* a type initializer runs where CoreCLR's `CEEInfo::initClass` runs it: at an access to a static
  field of its type, other than from that type's own initializer, and at a call to a method of its
  type that is static, a constructor, or an instance method of a value type or an interface, unless
  the type is marked `beforefieldinit`. A generic type's initializer runs for the instantiation
  the access or call names;
* every token a body names, a `catch` clause's included, is bound against the assemblies actually
  loaded, and a member or type that is not there contributes the `MissingMethodException`,
  `MissingFieldException` or `TypeLoadException` the JIT would throw. A member of one of the
  method's type variables (`!!0::Value`) exists or not depending on the instantiation, so binding
  it is "unknown". The type a type token names, each type argument of a method a call
  instantiates, the type of each local the body declares, the method's own return type and
  parameter types, and the return type of each method it calls (directly, through managed `calli`,
  or by `jmp`) with the type of each parameter of a call that is a value type, the JIT loads in
  full, as it does the owner of each member a token names (a method or field definition's
  declaring type, a member reference's parent) before it resolves the member, and the type of each
  field an instruction accesses, as the field's definition spells it: what each derives from and
  implements, the type of each of its value-type fields, transitively, and, for a type outside
  CoreLib, each type its type parameters' constraints and its generic methods' constraints name,
  must be there too (`TypeLoad`). A parameter
  of a class type it loads where the method called is compiled, which that method's own answer
  says. A local or parameter of a pointer, byref or function pointer type it does not load at all,
  nor what that points to (`JitTypeLoads`), unless Swift's calling convention calls the method:
  CoreCLR reads the method's `[UnmanagedCallersOnly]` conventions by the names of the types its
  `CallConvs` lists, loading none of them (`UnmanagedCallingConvention`), and Swift's is the one it
  reads where they name `CallConvSwift` and no second base convention; every target's JIT compiles
  Swift calls on the Unix runtime the analysis takes. Where CoreCLR cannot read those conventions,
  as it begins to compile the method, it raises, or for `CallConvs = null` kills the process,
  which the analysis does not name (`Opacity.UnreadableCallingConvention`). Before any of that, once the
  return type loads, CoreCLR refuses an `[UnmanagedCallersOnly]` method that is not static, is
  generic or declared in a generic type, or needs marshalling, with an `InvalidProgramException`,
  and the JIT loads none of the method's parameters' and locals' types: one carrying
  `[LCIDConversion]`, or whose return type or a parameter is `bool` or `char`, a reference, a
  byref, a pointer to a copy-constructed value type, or carries `[MarshalAs]`, with runtime
  marshalling enabled; a reference or a byref with it disabled. It walks the return type and then
  the parameters, loading in full each value type and instantiation it meets, so that one
  missing raises `TypeLoadException` there. Whether a value type that loads needs marshalling it
  decides by its layout, which the analysis does not model
  (`Opacity.ValueTypeMarshallingVerdict`). A return type of a pointer, byref or function pointer
  type it loads with what it points to. Of a call of unmanaged
  code (a P/Invoke, or `calli` by an unmanaged calling convention) the JIT loads what it loads of
  any call; where it makes the transition to unmanaged code in the caller's own code, it also
  loads each instantiation the VM's walk for whether the call needs marshalling reaches and, by
  Swift's convention, each pointer's target. CoreCLR reads the call's convention, as for
  `[UnmanagedCallersOnly]` (`UnmanagedCallingConvention`), from a P/Invoke's import, else its
  `[UnmanagedCallConv]`, else its signature's modifiers, or from a `calli` signature's calling
  convention and, for `unmanaged`, its modifiers. It does not make the transition there in a
  handler or a `try` with a `catch` or a filter, in an assembly whose `DebuggableAttribute`
  disables optimisation, by Fastcall's convention, or for a P/Invoke that does not preserve its
  signature, sets the last error or converts a locale, and the walk ends at the first type that needs marshalling by its kind (a
  class, an array, a byref, `string`, and with runtime marshalling `bool`, `char` or a spelt
  marshalling); elsewhere the analysis takes those to be loads it may make, as whether the JIT
  makes the transition turns on block weights. A class, a byref's target or, by another
  convention, a pointer's target it does not load there; the stub CoreCLR otherwise makes for the
  call loads them as the call runs, where the caller's handlers can catch the failure. Of a
  method whose address the body takes (`ldftn`, `ldvirtftn`), the JIT loads the return type in full, and CoreCLR loads each value
  type of the signature whose definition is there where it prepares the method as a function
  pointer: for `ldvirtftn` and a method a method specification instantiates, and otherwise where
  the JIT asks for the address of an `ldftn` whose address is not a delegate's target nor
  dropped. Making a delegate open over its target, as one over a static method is unless it closes
  over the method's first argument, CoreCLR makes a thunk to shuffle the arguments, loading
  approximately each value type of the delegate's `Invoke` signature and then of the target's: a
  class argument of an instantiation, or an interface's type arguments, need not be there. Making
  one closed over its target loads nothing more. For a static target that code shared between
  instantiations names through its type variables, CoreCLR makes the thunk as the delegate is made,
  where the method's own handlers can catch the failure, but the analysis takes it to fail the
  compilation there too. Before any of that, .NET 10 refuses a target that is
  `[UnmanagedCallersOnly]`, with a `NotSupportedException` that fails the compilation, where the
  JIT compiles the delegate's construction right after the `ldftn` or `ldvirtftn` naming it; .NET
  11 makes the delegate (`UnmanagedCallersOnly.refusesDelegateTarget`). The JIT asks for an `ldftn`'s address
  compiling whatever method it inlines the `ldftn` into, so what loading the value types then
  raises the analysis takes to escape every method that calls the body, directly or not, past all
  its handlers, as any of them may have the body inlined into it;
* CoreCLR on Unix, which the analysis answers for, refuses a signature with a variable argument
  list, managed or native, wherever it converts one for the JIT (`SignatureRefusal`): a `call`,
  `callvirt`, `newobj`, `ldftn` or `ldvirtftn` naming a method with one, or a `calli` whose call
  site has one, fails the compilation of the method holding it with `InvalidProgramException`,
  past all its handlers and before the JIT loads any type of the signature (CoreCLR prepares a
  method as a function pointer first for an `ldvirtftn`, or an `ldftn` of a method specification,
  which may fail before), unless resolving a `call` or `ldftn` of an abstract method refuses that
  first (`BadImageFormatException`); a method with one never compiles at all, so none of its body
  runs. `ldtoken` converts no signature. The call is not followed, nor anything it would call;
* of a call of unmanaged code, once it has loaded the return type, the JIT reads the calling
  convention (`UnmanagedConventionRead`), wherever in the code it compiles the call is: a `calli`
  signature by the `unmanaged` convention whose modifiers name two base conventions fails the
  compilation of the method holding it with `InvalidProgramException`, and a P/Invoke's
  `[UnmanagedCallConv]` CoreCLR cannot parse, or that says `CallConvs = null`, fails it with what
  the analysis does not name (`Opacity.UnreadableCallingConvention`); by Thiscall's convention, a
  call passing no argument fails it with `InvalidProgramException`. The call is not followed. A
  P/Invoke that is synchronized, or whose `[UnmanagedCallConv]` or signature's modifiers name two
  base conventions, or whose import or attribute names one convention and its signature's
  modifiers another, the VM refuses with a `TypeLoadException` where the JIT may begin its walk
  for whether the call needs marshalling;
* the JIT refuses to compile a method over an instruction for what its token names, wherever in
  the code it compiles the instruction is, whether a run would reach it or not: a `call` or
  `ldftn` naming an abstract method without a `constrained.` prefix (`BadImageFormatException`),
  a `newarr` of a byref-like element type (`TypeLoadException`), and a `box` of a byref-like type
  that it does not fold away (`InvalidProgramException`). It folds one followed in its block by a
  `brtrue` or `brfalse`, by `isinst` and then one of those where it decides the cast, or by an
  `unbox.any` (after an `isinst`, or not) of the boxed type. The code it compiles is what control
  reaches once it folds the branches on capability queries, so the answer has the refusal of an
  instruction behind any other branch. A call of an abstract method never runs, so it is not
  followed. Where a type variable names the type, the instance decides: in the open definition,
  where the parameter allows a byref-like type, it is "unknown". Shared code sees a byref-like
  type in its canonical form, which the JIT's comparisons of types do not decide, so it refuses
  the box there unless it inlines the method into a caller that sees the type as it is; the
  answer has the refusal;
* binding a token into another assembly that has a module initializer runs it, which may throw
  `TypeInitializationException`, and so does a `constrained.` call that dispatch lands in another
  assembly. A module initializer is a type initializer, and the answer has what it raises and
  what throwing that again raises, as for any other (below);
* a synchronized method takes a monitor around its body as the JIT has it, by running
  `Monitor.Enter(lock, ref acquired)`, with `acquired` false, before the body and
  `Monitor.ExitIfLockTaken(lock, ref acquired)` after it, and the answer has what those raise:
  releasing the monitor throws `SynchronizationLockException` if the body has already released
  it, either throws `ArgumentNullException` for an instance method `call`ed on null, and taking a
  contended monitor may allocate its sync block, or wait and be interrupted, and is "unknown",
  because a wait may call the thread's `SynchronizationContext.Wait`, which may be any override.
  The lock is the receiver, of
  the classes and nullness the caller passes, or for a static method its type's `RuntimeType`,
  which the JIT may make by calling `RuntimeTypeHandle.GetRuntimeTypeFromHandle`, as for
  `ldtoken` above.

  All these happen outside the body, so the body's own handlers do not catch them.

The answer is an over-approximation: whatever a run can let escape is in it, named or covered by
"unknown". Resource exhaustion is in it too, so almost every method can escape
`StackOverflowException`.

The runtime makes most of the exceptions it raises by itself (a null dereference's, a bounds
check's, a failed cast's, an overflow's) by running the exception's parameterless constructor,
which looks up its message, so what that constructor can raise is in the answer too:
`ThreadInterruptedException`, for one, from the lookup's lock. `OutOfMemoryException` for an
exhausted heap, and `StackOverflowException`, it allocated in advance, and makes without running
anything; an array allocation's `OutOfMemoryException` for lengths too large to allocate it makes
by the constructor. It makes the exception of a failure to bind a member
(`MissingMethodException`, `MissingFieldException`) or to dispatch a call
(`AmbiguousImplementationException`, `EntryPointNotFoundException`) the same way. A
multidimensional array constructor's `ArgumentOutOfRangeException` it makes the same way too, and
then looks up its message by a resource name (`Environment.GetResourceStringLocal`), which the
analysis follows as well. A failed type binding's `TypeLoadException` it makes by running a
constructor taking the type's name, which formats its message from the runtime's native resources
with `String.Format`, and what that constructor can raise is in the answer too. The analysis cannot
yet tell which formatting methods `String.Format` calls on the strings it is given, so an answer in
which a type fails to bind is "unknown".

Where a type initializer fails, the runtime runs `TypeInitializationException`'s
`(string, Exception)` constructor around what the initializer raised. Should that constructor fail,
the runtime raises the initializer's own exception instead, throwing it again, which runs
`Exception.InternalPreserveStackTrace` on it, and what that raises escapes in its place. So where an
initializer runs, the answer has the `TypeInitializationException`, what the initializer raises, and
what `InternalPreserveStackTrace` raises, but nothing the constructor does.

`EscapeAnalysis.unknownSources` says why an answer is "unknown". It lists the places (`OpaqueSite`)
where something the analysis cannot name may arise and then escape the method, past every handler
and through every `rethrow` on the way. Each gives the method the place is in, and the IL offset of
its instruction. There is no offset for a method with no IL the analysis follows, or for a binding
that fails before the body runs. Each also says why the analysis cannot see through it (`Opacity`),
and which method the instruction names, such as the virtual method a call names. The list is empty
exactly when the answer is not "unknown".

Casting an object to an interface, or storing it in an array (whose element type may be an
interface), calls `IDynamicInterfaceCastable.IsInterfaceImplemented` if the object's class
implements that interface. Where the analysis follows the CoreLib code that asks it, the cast
helpers' fallback, as it does from `castclass`, `isinst` and stores into arrays, that is the
assumption `Assumption.DynamicCastsAsDocumented`. The interface's
other callback, `GetInterfaceImplementation`, is reached only by an interface call or an interface `ldvirtftn` on
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

It takes CoreLib's `Object.GetType` to raise only `NullReferenceException` (called on null),
`OutOfMemoryException`, `StackOverflowException`, and on .NET 11 `NotSupportedException`, for the
continuation of a runtime-async method, whose type the runtime makes without metadata
(`ObjectGetType`). Its slow path, the QCall `RuntimeTypeHandle_GetRuntimeTypeFromHandleSlow`, has
no contract row, because given a type that is not fully loaded it finishes loading it, which can
bind an assembly and so run the program's `AssemblyLoadContext` resolution. An object's type is
fully loaded, though: CoreCLR allocates an object only of a type it has fully loaded, as the JIT
does for the type an instruction names, or of a type whose `RuntimeType` exists, and making that
loads the type fully or throws. `TestObjectGetType` holds the method's IL and the QCall's C++ to
that shape.

The same QCall is the slow path of `RuntimeTypeHandle.GetRuntimeTypeFromHandle`, which the JIT
calls to make a type's `RuntimeType` where it cannot embed it. Every type the JIT passes it is
fully loaded: one a generic dictionary holds, which CoreCLR loads fully to fill the slot; a type
argument of the exact class or method the code runs for, which loading that fully loads too; or
that class itself, which CoreCLR loads fully before the code runs. So where the JIT makes the call,
the analysis takes it to raise only `OutOfMemoryException` and `StackOverflowException`, and runs
none of the method's IL. CoreLib's own calls of the method pass handles nothing promises are fully
loaded, so they run its IL, and reach the QCall.

It assumes that CoreLib's lookup of its own resource strings, `SR.InternalGetResourceString`, which
gives every CoreLib exception its message, raises only `OutOfMemoryException`,
`TypeInitializationException`, `NullReferenceException` (for a null key),
`ThreadInterruptedException` (while it waits for its lock) and `StackOverflowException`. That holds
when:

* only CoreLib writes CoreLib's private static fields. Reflection could otherwise replace the
  lookup's resource manager with a subclass whose `GetString` throws anything;
* the runtime is installed completely and intact. A damaged resources file would otherwise throw
  from the reader;
* every culture the lookup walks (the current UI culture and each one its `Parent` chain reaches)
  that is of a `CultureInfo` subclass the program defines behaves as CoreLib's own `CultureInfo`
  would for that culture: its overrides raise nothing, its `Name` is a valid culture name, its
  `Parent` chain ends at the invariant culture, and it changes no culture state. The lookup reads
  each culture's `Name` and `Parent`; what an override throws there escapes the exception's
  constructor, and a name the lookup rejects raises `ArgumentException`.

The program's `AssemblyResolve` and `Resolving` handlers do not run there: CoreCLR runs none for
CoreLib's own satellite assembly. Reading the code could not establish these conditions instead:
whether the installation is intact is a fact about the machine, whether some code writes a private
field by reflection is undecidable, and any code can make an object of any subclass the current UI
culture. Without them, constructing almost any CoreLib exception would be "unknown", because the
lookup reaches culture data, formatting, collections, event tracing and reflection.

Constructing a CoreLib exception runs type initializers, though: `SR`'s, as it asks for its message,
and the resource reader's types', in the lookup. Should one fail, the runtime may throw its own
exception again, which reads that exception's stack trace. The analysis cannot see into `SR`'s
initializer or into that read, so unless the caller allows `CoreLibTypeInitializers` and
`StackTracePreserved` (each an `Assumption`), constructing almost any CoreLib exception is "unknown" still.

Other assumptions are the caller's to allow (`Assumption`, given to `EscapeAnalysis.create`). Each
lets the analysis take a contract in place of a method's body, or what the JIT compiles in place of
code, and each answer lists in `Escapes.Assumes` those it relied on: the ones whose contract stands
in for code that something escaping could come from, so that without them the answer could be
"unknown". It may also list one that a handler in a method this one calls made unnecessary, since a
callee's answer records only that it relied on the assumption. Allowed none, every answer follows
from the code and the assumptions above. Each case of `Assumption` says what it claims, when that
holds, and what is "unknown" without it.

That holds for a set of assemblies that agree with each other. When one has changed since another
was compiled against it, a missing member or type is reported as above, and says that the set
needs rebuilding (or, for a package, a different version). A member made inaccessible to a
caller is "unknown" wherever its visibility does not settle the check, unless the caller allows
`MembersAccessible`.

A generic instantiation that a constraint added since now rejects is reported where the JIT checks
it, before the method runs:
- **Types:** every instantiation the full load of a type the JIT loads in full reaches, where that
  load meets no missing type, raises `TypeLoadException` if an argument breaks its constraints
  (`ConstraintSatisfaction`). The parts the load reaches are the base, the interfaces and the
  value-type fields that `TypeLoad.fullLoadParts` lists, each instantiated, and the generic
  arguments.
- **Methods:** a method specification whose arguments break the method's constraints raises
  `VerificationException`.
- **Malformed arguments:** a byref, pointer, `TypedReference` or `void` argument raises
  `TypeLoadException` for a type and `BadImageFormatException` for a method.

What code shared between instantiations names through its type variables, the JIT sees in its
canonical form (`System.__Canon` for a reference type). CoreCLR does not check an instantiation
containing `__Canon`, though it checks what the load of one brings that does not contain it.

These checks are exact where the instantiation is closed, but some are left open:
- **Instantiations over a generic method's own type variables:** CoreCLR also checks an
  instruction it checks access of, in a generic method or a method of a generic type, with the
  method's own type variables as the arguments. The analysis does not model that check, and is
  "unknown" there (`Opacity.ConstraintsOverTypeVariables`) wherever the instruction's token
  instantiates, over those variables, something whose constraints, or whose full load's, could
  reject them.
- **A definition analysed for every instantiation at once:** it is "unknown"
  (`Opacity.DependsOnInstantiation`) wherever some instantiation of its type variables may break a
  constraint the JIT checks.

  In both, the analysis takes an argument over the variables as satisfying a constraint, for
  every instantiation, only where that follows from its shape. A bare variable satisfies it where
  the variable's own constraints already demand at least as much: `class`, `struct` and `new()`
  where its own say as much (`struct` gives `new()`, and a constraint to a class other than
  `Object`, `ValueType` and `Enum` gives `class`); a byref-like argument only where the variable
  allows none, or the parameter allows one; and each type constraint where it is among the bases
  and interfaces of the variable's own constraints. Another argument satisfies it by what its
  definition is, and by its own bases and interfaces, though an abstract class or an interface
  does not satisfy an interface constraint that declares abstract static virtual methods. A
  pointer, a byref or a function pointer satisfies nothing, even in code shared between
  instantiations, where CoreCLR checks an argument's form though not its constraints.
- **The static-virtual rule:** an abstract argument against an interface constraint that declares
  abstract static virtual methods (`Opacity.UndecidedConstraint`).
- **Types TypeSystem cannot instantiate:** a load reaching a type whose argument carries a custom
  modifier, or a function pointer naming a type that is not there, leaves its checks undecided
  (`Opacity.UndecidedConstraint`), unless no generic definition it names, nor any their loads
  bring, has a constrained type parameter, or one not allowing a byref-like argument given one
  that may be. Binding goes on past an undecided check as though it passed.
- **A generic type's method instantiated by its definition token:** IL that C# does not write, a
  method specification over a `MethodDef` of a generic type's method, or over a `MemberRef` whose
  parent names the generic type without arguments, names the method on the type's typical
  instantiation, whose arguments are its own type parameters. Where the method's constraints, or
  a type of its signature the JIT loads, mention type variables, the check is undecided
  (`Opacity.UndecidedConstraint`).
- **Order within one load:** a load that misses is reported as the miss, which is exact for the
  instantiation whose definition misses, as CoreCLR loads a definition's constraints first. Within
  one load the analysis does not keep CoreCLR's order. Where CoreCLR checks an instantiation before
  it reaches a later part that misses, it raises that check's `TypeLoadException`, where the
  analysis reports the miss.
- **A generic definition spelled bare:** this is its typical instantiation, which CoreCLR checks
  against its own type parameters; the analysis does not check it.
- **Run-time lookups:** where shared code looks a type up as it runs, as `ldtoken` does, a violation
  is raised there, inside the method; the analysis does not report it.

Each method is taken to be compiled by the JIT when it is first called. Code compiled ahead of time
(ReadyToRun: the shared framework's, and a program's published that way) binds what it names as it
runs instead, so that a failure the analysis reports as escaping a method past all its handlers is
raised there at the instruction, where the method's own handlers can catch it, or not at all on a
path that does not run. CoreCLR also loads no constraints in a ReadyToRun image that skips type
validation; the analysis loads them in every assembly outside CoreLib, which can only add
exceptions that may escape.

An assembly that is not there at all, where a type was missing from one that is, is reported as
`FileNotFoundException` wherever the JIT loads a type in it that a token, a local, a `catch`
clause, the method's own signature, the signature of a method it calls or takes the address of,
or the type of a field it accesses names, or that binding a member reference compares in its
signature, and as "unknown": before raising that, the runtime runs the program's `AssemblyLoadContext.Resolving` and
`AppDomain.AssemblyResolve` handlers, which may throw (the runtime then raises a `FileLoadException`
wrapping what they threw), or load an assembly in its place whose code then runs.

This package sees `WoofWare.PawPrint.Domain`, `WoofWare.PawPrint.Loader`,
`WoofWare.PawPrint.TypeSystem` and `WoofWare.PawPrint.Semantics`, and never the interpreter.
