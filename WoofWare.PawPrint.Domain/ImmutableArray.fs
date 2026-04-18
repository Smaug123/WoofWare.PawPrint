namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module internal ImmutableArray =

    let inline map ([<InlineIfLambda>] f : 'a -> 'b) (arr : ImmutableArray<'a>) : ImmutableArray<'b> =
        let b = ImmutableArray.CreateBuilder ()

        for i in arr do
            b.Add (f i)

        b.ToImmutable ()

    let inline mapi ([<InlineIfLambda>] f : int -> 'a -> 'b) (arr : ImmutableArray<'a>) : ImmutableArray<'b> =
        let b = ImmutableArray.CreateBuilder ()

        for i = 0 to arr.Length - 1 do
            b.Add (f i arr.[i])

        b.ToImmutable ()
