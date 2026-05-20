namespace WoofWare.PawPrint

type HardwareIntrinsicsProfile =
    {
        Vector128 : bool
        Vector256 : bool
        Vector512 : bool
    }

[<RequireQualifiedAccess>]
module HardwareIntrinsicsProfile =
    let ScalarOnly : HardwareIntrinsicsProfile =
        {
            Vector128 = false
            Vector256 = false
            Vector512 = false
        }
