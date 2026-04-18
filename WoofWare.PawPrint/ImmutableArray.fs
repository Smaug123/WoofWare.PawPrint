namespace WoofWare.PawPrint

open System.Collections.Immutable

[<RequireQualifiedAccess>]
module internal ImmutableArray =

    let map (f : 'a -> 'b) (arr : ImmutableArray<'a>) : ImmutableArray<'b> =
        let b = ImmutableArray.CreateBuilder ()

        for i in arr do
            b.Add (f i)

        b.ToImmutable ()
