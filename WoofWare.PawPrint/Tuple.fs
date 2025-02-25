namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module internal Tuple =
    let withLeft<'a, 'b> (x : 'a) (y : 'b) : 'a * 'b = x, y
    let withRight<'a, 'b> (y : 'b) (x : 'a) = x, y
