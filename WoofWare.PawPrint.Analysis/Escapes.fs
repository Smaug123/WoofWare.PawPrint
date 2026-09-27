namespace WoofWare.PawPrint.Analysis

open System.Reflection.Metadata
open WoofWare.PawPrint

/// A method definition, identified across every assembly an analysis has loaded: the definition
/// identity of the assembly that declares it, and its MethodDef row there.
type MethodKey =
    {
        AssemblyFullName : string
        Method : ComparableMethodDefinitionHandle
    }

    override this.ToString () : string =
        $"%s{this.AssemblyFullName}/%O{this.Method.Get}"

[<RequireQualifiedAccess>]
module MethodKey =
    let make (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : MethodKey =
        {
            AssemblyFullName = assembly.DefinitionFullName
            Method = ComparableMethodDefinitionHandle.Make method
        }

/// The type of an exception that may escape.
///
/// IL may throw an object that is not an exception. Such an object is named as itself here; a
/// <c>catch</c> or <c>filter</c> clause sees it as <c>RuntimeWrappedException</c> instead if its
/// method's assembly wraps non-exception throws (<c>RuntimeCompatibility.wrapsNonExceptionThrows</c>).
[<RequireQualifiedAccess>]
type ThrownType =
    /// An exception of exactly this type: one constructed with `newobj` and thrown, or raised by the
    /// runtime.
    | Exactly of ResolvedTypeIdentity
    /// An exception of this type or of any type derived from it: what is known of a value thrown
    /// when only its static type is, such as the result of a helper that returns the exception.
    | SubtypeOf of ResolvedTypeIdentity

/// What may escape a method.
///
/// <c>Types</c> are the exception types the analysis can name. <c>Unknown</c> says that somewhere
/// on a path the analysis could not see through (a virtual call, a native method, a <c>rethrow</c>),
/// so anything at all may escape besides; with it false, <c>Types</c> is a complete answer.
type Escapes =
    {
        Types : Set<ThrownType>
        Unknown : bool
    }

[<RequireQualifiedAccess>]
module Escapes =
    /// Nothing escapes.
    let none : Escapes =
        {
            Types = Set.empty
            Unknown = false
        }

    /// Everything either may let escape.
    let union (a : Escapes) (b : Escapes) : Escapes =
        {
            Types = Set.union a.Types b.Types
            Unknown = a.Unknown || b.Unknown
        }
