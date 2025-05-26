namespace WoofWare.PawPrint.ExternImplementations

type NativeImpls =
    {
        System_Environment : ISystem_Environment
        System_Threading_Monitor : ISystem_Threading_Monitor
    }

    static member PassThru () =
        {
            System_Environment = System_Environment.passThru
            System_Threading_Monitor = System_Threading_Monitor.passThru
        }

    interface ISystem_Environment_Env with
        member this.System_Environment = this.System_Environment

    interface ISystem_Threading_Monitor_Env with
        member this.System_Threading_Monitor = this.System_Threading_Monitor
