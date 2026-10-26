# native-call-graph

Writes, for each runtime `flake.nix` pins and each of Linux x64 and Linux arm64, the call graph of the C++ that
`WoofWare.PawPrint.Semantics/NativeContractTable.tsv` cites, as clang parses a release build of CoreCLR. The graphs
are checked in at `WoofWare.PawPrint.Test/NativeCallGraph/NativeCallGraph.<runtime>.<target>.tsv`, and the flake check
`native-call-graph-<runtime>-<target>` fails unless each is what this directory generates now. To regenerate one:

```bash
nix build .#native-call-graph-net10-linux-x64
cp result/NativeCallGraph.net10.linux-x64.tsv WoofWare.PawPrint.Test/NativeCallGraph/
```

## What a graph says

Its header names the runtime's commit, the target, the compiler and the configure flags. Then its rows, by their
first field:

* `routine KEY NAME SIGNATURE USR PATH:LINE`: a definition of a requested routine. A key is a citation as the
  contract table writes it in its sources, or a vouch's callee as `NativeContractCallees.tsv` writes it, where either
  names a file of C or C++. A definition matches a key when it is in the file the key names and its fully qualified
  name is the key's symbol or ends in `::` and the symbol, as the tables' own reader matches: `GCHeap::GetLOHThreshold`
  matches both `WKS::GCHeap` and `SVR::GCHeap`'s definitions in `gc/gc.cpp`. A template's key, which names it without
  arguments, matches each of its instantiations.
* `uninstantiated KEY`: the key names a template no unit instantiates.
* `undefined KEY`: no unit defines a function the key names. Most are macros.
* `call ROUTINE-NAME ROUTINE-SIGNATURE ROUTINE-USR PATH:LINE MACRO KIND CALLEE-NAME CALLEE-SIGNATURE CALLEE-USR ENUMS`:
  one call a routine's definition makes.

Names are qualified fully, signatures are canonical types, and paths are relative to the runtime's root, or to the
sysroot (`sysroot/`) or clang's resource directory (`clang/`). `PATH:LINE` is where a reader of the file sees the call:
at the outermost macro's invocation for a call a macro's body makes, and where it is written for one written in a
macro's argument. `MACRO` is `PATH:NAME` of that outermost macro, or `-`. `ENUMS` lists each argument naming an
enumerator, as `N=Enum::Enumerator`, numbering arguments from 1 and counting defaults.

The kinds of call:

* `direct`: to the function named, including a virtual one called with a qualifier (`X::F()`), and one whose
  receiver's dynamic class clang can tell, as a final class's or a local object's, which it calls directly.
* `virtual`: one row for each function the call's vtable slot holds, in each class the program defines that is, or
  derives from, the receiver's static class and is not abstract, read from clang's layout of that class's vtable at
  each subobject of the receiver's class. The program is every unit of `libcoreclr.so` and `libclrgc.so`, so a class
  that only another binary defines is not seen.
* `virtual-unimplemented`: a virtual call no class the program defines can take; the callee is the method named.
* `indirect`: through a pointer to a function or to a member function. The callee is `-`, and the signature the
  pointer's.
* `construct`, `destruct`: a constructor or destructor that is not trivial, and whose call clang does not elide.
  Destructors are those of every automatic variable, every temporary, a destructor's bases and members, and those of
  a constructor's bases and members that it destroys when a later initializer or its body throws.
* `new`, `delete`: the `operator new` or `operator delete` a `new` or `delete` calls, and the `operator delete` a `new`
  calls to free its memory when its initializer throws. A `delete` of a class with a virtual destructor is a `virtual`
  call of the destructor, with a `delete` row for the `operator delete` of each class it can reach.
* `builtin`: a builtin that clang lowers rather than calls, as `__builtin_expect` or `std::move`.
* `throw`: a `throw`, whose callee is the type thrown, or `-` for a rethrow.
* `asm`: inline assembly, whose calls, if any, are not seen.

A call to a trivial function, as a trivial assignment operator, is none. Nothing in an unevaluated operand (`sizeof`,
unless of a variable-length array, `decltype`, `noexcept`, a `typeid` of a type that is not polymorphic), a constant
expression, the initializer of a variable that is initialized as a constant, a `static_assert`, or a branch clang
leaves out because its condition is a constant (the discarded branch of an `if constexpr`, and that of an `if`, `?:`,
`&&` or `||` whose condition folds and whose other branch holds no label) is a call. A default argument's or default
member initializer's calls are made where it is used. A lambda's or a local class's body counts as its enclosing
routine's; a generic lambda's, as each of its instantiations.

## How the graph is checked against clang's IR

As it walks a unit, the tool also has clang emit the unit's LLVM IR, as clang's front end emits it before any
optimisation, with every requested routine's definition emitted even where the unit does not use it. For each
routine it compares the IR of its definition (every function clang emits for it but the deleting destructor, with the
functions defined inside it) with the walk, and fails on any difference, naming the routine and what differs:

* the functions the IR calls by name must be the walk's `direct`, `construct`, `destruct`, `new` and `delete` callees,
  cleanup paths included, but that a call of `memcpy`, `memmove` or `memset` may be lowered to its LLVM intrinsic;
* the IR's calls through a pointer, and its inline assembly, must be as many as the walk's `virtual`, `indirect` and
  `asm` sites;
* the C++ runtime's functions the IR calls must be accounted for by what the walk found: `__cxa_allocate_exception`,
  `__cxa_throw` and `__cxa_free_exception` by a throw, `__cxa_rethrow` by a rethrow, `__cxa_begin_catch`,
  `__cxa_end_catch` and `__cxa_get_exception_ptr` by a catch, `__cxa_guard_*` by a static local initialized at run
  time and `__cxa_atexit` by one destroyed at exit; each is required where the walk found its cause, and
  `__clang_call_terminate` is always allowed. An LLVM intrinsic is not a callee; any other function the IR calls fails
  generation.

Generation fails unless some unit's IR was compared with the walk of every routine. The IR cannot tell what a virtual
call reaches: it shows only a call through a pointer. What `virtual` rows name rests on clang's vtable layout, and on
the oracle.

## How it is made

`default.nix` builds, for each runtime and target:

1. A sysroot of the target's glibc, libstdc++, lttng-ust and liburcu, from nixpkgs's packages for that target,
   substituted rather than built.
2. A CMake configure of `src/coreclr`, from the flake's sparse checkout with `coreclr-configure.patch` applied, using
   the runtime's own `eng/native/tryrun.cmake`, with clang and lld as the cross-compiler. `program.py` then generates
   the files the units read that the build itself writes, such as the eventing headers, and lists the units of the two
   libraries. Nothing is compiled.
3. `requests.py` reads the routines to describe from the two tables. `generate.py` runs `callgraph.cpp`, a LibTooling
   program, over every unit twice: for the routines' calls, checked against the IR, and then for the functions each
   virtual call can reach.

`oracle/` is a small program with every case above, and `oracle/expected.tsv` its graph, less its USRs; the flake check
`native-call-graph-oracle` holds the generator to it. `native-call-graph-cross-check` builds the tool with its walk
wrong, leaving out a throwing `new`'s cleanup, and requires the comparison with the IR alone to refuse it.
