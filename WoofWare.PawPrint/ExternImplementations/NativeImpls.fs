namespace WoofWare.PawPrint.ExternImplementations

type NativeImpls =
    {
        System_Environment : ISystem_Environment
    }

    static member PassThru () =
        {
            System_Environment = System_Environment.passThru
        }

    interface ISystem_Environment_Env with
        member this.System_Environment = this.System_Environment
