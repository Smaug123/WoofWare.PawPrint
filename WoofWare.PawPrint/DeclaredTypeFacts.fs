namespace WoofWare.PawPrint

open System.Runtime.InteropServices

/// <summary>
/// The facts about a declaring type that <c>CliValueType</c> construction needs: how its fields are
/// placed, how its strings marshal, and whether it is a CLR enum.
/// </summary>
/// <remarks>
/// <para>
/// These are bundled rather than passed individually because they all come from the same
/// <c>TypeInfo</c>, and every construction site needs all of them. Deriving them separately at each
/// site invites the sites to disagree: <c>TypeLayoutKind.applied</c> needs to know whether the type
/// is a value type, so a site that hard-codes that answer while its sibling computes it has already
/// diverged.
/// </para>
/// <para>
/// <c>IsEnum</c> is the reason this type exists at all. Enum-ness is nominal — the immediate base
/// type is <c>System.Enum</c> — so answering it needs metadata, and
/// <c>CliValueType.ClassifyPrimitiveLike</c> has none: it sees a <c>ConcreteTypeHandle</c> and a
/// field list. Before issue #996 it guessed structurally, from the CLR-reserved field name
/// <c>value__</c>, and <c>struct Fake { public int value__; }</c> is legal C# that defeats the
/// guess. Note the question cannot be answered lazily at construction either:
/// <c>IlMachineRuntimeMetadata.isEnumValueType</c> returns an updated machine state because
/// resolving a base type may load an assembly, and <c>CliValueType.OfFields</c> returns a bare
/// value with nowhere to put one. So the answer must be computed by a caller that holds the load
/// context, and handed in.
/// </para>
/// <para>
/// Prefer <c>ofTypeInfo</c> and <c>ofCorelibType</c> to a record literal: they are what make the
/// five facts consistent with each other. A literal cannot be prevented — F#'s <c>private</c> is
/// assembly-scoped — so <c>CliValueType.OfFields</c> additionally asserts that a value claiming
/// <c>IsEnum</c> really has an enum's field shape.
/// </para>
/// </remarks>
type DeclaredTypeFacts =
    {
        /// Whether the type derives from `System.ValueType` (and is not itself `ValueType` or
        /// `Enum`). Reference types reach `CliValueType` too: a heap object's field block is one.
        IsValueType : bool
        /// Whether the type's immediate base is `System.Enum`. False for `System.Enum` itself.
        IsEnum : bool
        /// Which field-placement algorithm governs the type, as `TypeLayoutKind.applied` reports
        /// it — i.e. after the correction that a reference type declaring `Auto` is laid out
        /// sequentially by PawPrint today (issue #994).
        LayoutKind : TypeLayoutKind
        /// The `ClassLayout` table's `Pack`/`Size` for the type, which only `Sequential` and
        /// `Explicit` placement read.
        Layout : Layout
        /// Marshalling string-encoding hint from `TypeAttributes.StringFormatMask`.
        CharSet : CharSet
    }

[<RequireQualifiedAccess>]
module DeclaredTypeFacts =

    /// Derive every fact from one `TypeInfo`, given a load context that can resolve its base chain.
    ///
    /// The base walk is the same one `DumpedAssembly.isValueType` performs, and every call site
    /// that reaches here was already performing it, so this adds no new failure mode: a base type
    /// naming an assembly that is not loaded fails here exactly as it already failed there.
    let ofTypeInfo
        (bct : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (ti : TypeInfo<'generic, 'field>)
        : DeclaredTypeFacts
        =
        let isValueType = DumpedAssembly.isValueType bct assemblies ti

        {
            IsValueType = isValueType
            IsEnum = DumpedAssembly.isEnum bct assemblies ti
            LayoutKind = TypeLayoutKind.applied isValueType ti.TypeAttributes
            Layout = ti.Layout
            CharSet = CharSetMetadata.ofTypeAttributes ti.TypeAttributes
        }

    /// Derive every fact for a type defined in corelib, without a load context.
    ///
    /// The handle registries build fixed BCL types (`RuntimeType`, `RuntimeFieldHandle`,
    /// `IntPtr`, ...) at sites that hold `BaseClassTypes` but no `LoadedAssemblies`. Corelib
    /// references no other assembly, so a corelib type's entire base chain is inside corelib and a
    /// one-assembly load context answers every question `ofTypeInfo` asks. The assertion is what
    /// keeps that reasoning true: pass a non-corelib type and this fails loudly rather than
    /// silently walking a base chain it cannot resolve.
    let ofCorelibType (bct : BaseClassTypes<DumpedAssembly>) (ti : TypeInfo<'generic, 'field>) : DeclaredTypeFacts =
        if ti.Assembly.FullName <> bct.Corelib.Name.FullName then
            failwith
                $"DeclaredTypeFacts.ofCorelibType: %s{ti.Namespace}.%s{ti.Name} is defined in %s{ti.Assembly.FullName}, not in corelib (%s{bct.Corelib.Name.FullName}); its base chain may leave corelib, so it needs a full load context via ofTypeInfo"

        ofTypeInfo bct (LoadedAssemblies.ofAssemblies [ bct.Corelib ]) ti
