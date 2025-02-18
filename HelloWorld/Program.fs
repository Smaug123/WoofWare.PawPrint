namespace HelloWorld

module Program =
    let reallyMain argv =
        System.Console.WriteLine "Hello, world!"
        0

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
