namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// What a `TypeInfo` knows about the assembly that declares it, and what that costs to read.
///
/// Corelib is the fixture on purpose: it is the only assembly in reach that carries a public key,
/// and `AssemblyName.FullName` derives the public key *token* from the key by SHA-1 on every call.
/// A type-keyed lookup that spells its key as `AssemblyName.FullName` therefore hashes corelib's
/// public key once per lookup, which is what the allocation budget below is watching for.
[<TestFixture>]
module TestTypeInfoAssemblyIdentity =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory typeof<obj>.Assembly.Location

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    /// `TypeInfo.isBaseType` asks "is this type one of the base classes?", and the assembly half of
    /// that question is about *which assembly*, not about which object holds the answer. A caller
    /// that assembled the identity itself must be told the same thing as one passing the identity
    /// corelib was read with, or it hears that corelib's `System.Object` is an ordinary type.
    [<Test>]
    let ``isBaseType answers by assembly identity, not by object identity`` () =
        let getName (a : DumpedAssembly) : string = a.DefinitionFullName

        // Equal contents, deliberately not the same instance.
        let sameIdentityAnotherInstance =
            System.String (corelib.DefinitionFullName.ToCharArray ())

        Object.ReferenceEquals (sameIdentityAnotherInstance, corelib.DefinitionFullName)
        |> shouldEqual false

        for identity in [ corelib.DefinitionFullName ; sameIdentityAnotherInstance ] do
            TypeInfo.isBaseType bct getName identity bct.Object.TypeDefHandle
            |> shouldEqual (Some ResolvedBaseType.Object)

            TypeInfo.isBaseType bct getName identity bct.ValueType.TypeDefHandle
            |> shouldEqual (Some ResolvedBaseType.ValueType)

        // And it must still say "no" to a genuinely different assembly, and to a corelib row that
        // is not one of the base classes.
        TypeInfo.isBaseType bct getName "Some.Other.Assembly" bct.Object.TypeDefHandle
        |> shouldEqual None

        TypeInfo.isBaseType bct getName corelib.DefinitionFullName bct.String.TypeDefHandle
        |> shouldEqual None

    /// A type's own rendering names its assembly by *simple* name. Nothing here depends on the
    /// version or public key of corelib, and a display name in every diagnostic would bury the
    /// type. The expectation comes from the host CLR rather than from the assembly under test.
    [<Test>]
    let ``a type renders with its assembly's simple name`` () =
        let expected =
            $"%s{typeof<obj>.Assembly.GetName().Name}.%s{typeof<obj>.Namespace}.%s{typeof<obj>.Name}"

        string<TypeInfo<GenericParamFromMetadata, TypeDefn>> bct.Object
        |> shouldEqual expected

    /// Reading a type's identity must not *derive* anything: it is asked on every type-keyed
    /// lookup, so the assembly half has to be a value the `TypeInfo` already holds rather than one
    /// serialised afresh. The budget admits the two small records an identity is made of and
    /// nothing like the cost of re-deriving corelib's public key token.
    [<Test>]
    let ``reading a type's identity derives nothing`` () =
        let reads = 10_000

        // The identities must escape, or .NET's escape analysis is free to stack-allocate them and
        // the measurement below reports less than the code really costs.
        let sink = Array.zeroCreate<ResolvedTypeIdentity> reads

        // Warm first: the JIT, and whatever the first read of this type faults in.
        for i in 0..99 do
            sink.[i] <- bct.Object.Identity

        let before = GC.GetAllocatedBytesForCurrentThread ()

        for i in 0 .. reads - 1 do
            sink.[i] <- bct.Object.Identity

        let allocated = GC.GetAllocatedBytesForCurrentThread () - before
        let perRead = allocated / int64 reads

        TestContext.Out.WriteLine $"%d{reads} identity reads allocated %d{allocated} bytes, %d{perRead} per read"

        // The answers really were computed, and all agree.
        sink |> Array.forall (fun i -> i = bct.Object.Identity) |> shouldEqual true

        perRead |> shouldBeSmallerThan 128L
