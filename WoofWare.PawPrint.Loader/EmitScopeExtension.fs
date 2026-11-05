namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata.Ecma335

/// <summary>
/// One way in which a later emit scope fails to extend an earlier one, so that an image of the later
/// scope could not replace the earlier image without changing something the runtime has already read.
/// </summary>
/// <remarks>
/// A row's <c>position</c> is its RID in the scope, which is its RID in the image, except in
/// <c>InterfaceImpl</c> and <c>MethodImpl</c>: there it is the 1-based position at which the scope
/// added the row, because the image sorts those tables by class.
/// </remarks>
[<RequireQualifiedAccess>]
type EmitScopeExtensionViolation =
    /// A row of the earlier scope differs in the later one, or the later scope does not have it.
    | RowChanged of table : TableIndex * position : int

    /// An entry of one of the earlier scope's heaps differs in the later one, or the later scope does
    /// not have it. <c>entry</c> counts from 1 in the order the entries were interned, the empty
    /// string or blob not counted.
    | HeapEntryChanged of heap : HeapIndex * entry : int

    /// A row the later scope adds is a member of a type already created: a field or method it
    /// declares, a parameter of one of its methods, or one of its interface implementations or
    /// method impls.
    | AddedToCreatedType of table : TableIndex * position : int * createdType : TypeDefRid

    /// A row of a created type, in a table the image sorts, has a different RID in the later image
    /// than in the earlier one, because the later scope added a row for a type that sorts before it.
    | SortedRowMoved of table : TableIndex * position : int * createdType : TypeDefRid * before : int * after : int

/// Whether one emit scope extends another, given which of its types the runtime has already created.
[<RequireQualifiedAccess>]
module EmitScopeExtension =

    /// <summary>
    /// Every way in which <paramref name="after"/> fails to extend <paramref name="before"/>.
    /// </summary>
    /// <remarks>
    /// Empty when <paramref name="after"/> was built from <paramref name="before"/> by appending rows
    /// and interning entries, none of them for a created type and none sorting before a created type's
    /// rows in a sorted table. Then an image of <paramref name="after"/> reads back every row, heap
    /// entry and created type of <paramref name="before"/>'s as <paramref name="before"/>'s image
    /// does. A history that was rewritten, rather than appended to, is refused even where the image
    /// happens to read back the same.
    /// </remarks>
    /// <param name="created">
    /// The types of <paramref name="before"/> the runtime has created, whose members are fixed from
    /// then on.
    /// </param>
    /// <param name="before">The scope whose image the runtime has read.</param>
    /// <param name="after">The scope whose image is to replace it.</param>
    let violations
        (created : ImmutableHashSet<TypeDefRid>)
        (before : EmitScope)
        (after : EmitScope)
        : EmitScopeExtensionViolation list
        =
        failwith "TODO: EmitScopeExtension.violations"
