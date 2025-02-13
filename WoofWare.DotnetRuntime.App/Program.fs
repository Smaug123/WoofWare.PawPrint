namespace WoofWare.DotnetRuntime

module Program =
    let reallyMain (argv : string[]) : int = 0

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
