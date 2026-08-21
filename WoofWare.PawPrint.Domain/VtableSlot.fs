namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// The type a vtable slot's occupant was read from, reduced to what deciding the layout needs: the
/// token space its signature is spelled in, the identity that orders ties by derivation, and the
/// substitution its `!i` are read against. The base chain's entries carry a different substitution
/// from the derived type's -- which is the whole difficulty of matching an override against the slot
/// it fills.
type SlotOwner =
    {
        AssemblyFullName : string
        Identity : ResolvedTypeIdentity
        Substitution : TypeConcretization.SubstitutionContext
        /// How to name this type in a diagnostic. Held rather than derived, because the walk that
        /// builds it knows whether it is looking at an instantiation or at a definition and the
        /// identity alone does not carry a name.
        Description : string
    }

/// One entry of a type's instance vtable: the method occupying the slot, together with the type it
/// was read from.
///
/// Lives here rather than beside the walk that computes it because `IlMachineState` memoises those
/// walks and compiles well before them: a cache of `VirtualSlotLayout`'s results cannot be typed in
/// a file that `VirtualSlotLayout` has not reached yet.
type VtableSlot =
    {
        Method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        DeclaredBy : SlotOwner
    }

/// What identifies a vtable slot's occupant well enough to find it again: the full name of the
/// assembly that declares the method, paired with the method's within-assembly identity.
///
/// The assembly is not decoration. `MethodInfo.IdentityKey` is a MethodDef *row number*, unique only
/// within its own module, and a vtable routinely spans assemblies -- a guest type deriving from
/// `System.Object` has corelib's rows sitting underneath its own.
type SlotIdentity = string * (MethodDefinitionHandle option * SynthesisedMethod option)

/// A type's method table as virtual dispatch reads it.
///
/// Both halves are indexed rather than listed because dispatch consults both once per `callvirt`,
/// and over lists they are a linear scan and a linear walk -- `System.Int32` declares around 110
/// virtuals. Measured on the dispatch-saturated benchmark guest: listed, 218.2ms; indexed, 193.7ms.
type DispatchTable =
    {
        /// The slot each declaration in this type's chain owns: `MethodDesc::GetSlot()`. More than
        /// the occupants below, because a declaration a derived type overrode by placement still owns
        /// its slot while no longer occupying it.
        SlotOfDeclaration : ImmutableDictionary<SlotIdentity, int>
        /// What each slot holds, indexed by slot number: the method a `callvirt` through that slot
        /// runs.
        ///
        /// Immutable, not a bare array. `IlMachineState` is a snapshot that forks share, and this table
        /// is memoised inside one -- so a mutable array here would let a single write retroactively
        /// change which method every past and future `callvirt` resolves to, in every state sharing the
        /// cache. That is precisely the property a deterministic replay cannot afford.
        Occupants : ImmutableArray<VtableSlot>
    }
