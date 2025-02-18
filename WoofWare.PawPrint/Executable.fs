namespace WoofWare.PawPrint

open System
open System.Text

type MsIlInstruction = | Something

type Characteristics =
    {
        Is32BitMachine : bool
        IsFileDll : bool
    }

    static member Parse (b : ReadOnlySpan<byte>) : Characteristics option =
        if b.[0] &&& 0x1uy <> 0uy then
            None
        elif b.[0] &&& 0x2uy <> 0x2uy then
            None
        else

        {
            Is32BitMachine = b.[1] &&& 0x1uy = 1uy
            IsFileDll = b.[1] &&& 0x20uy = 0x20uy
        }
        |> Some

type PeHeaderStandardFields =
    {
        CodeSize : uint32
        LMajor : byte
        LMinor : byte
        InitialisedDataSize : uint32
        UninitialisedDataSize : uint32
        EntryPointRva : uint32
        BaseOfCode : uint32
        BaseOfData : uint32
    }

    static member Parse (b : ReadOnlySpan<byte>) : PeHeaderStandardFields option =
        if toUint16 (b.Slice (0, 2)) <> 0x10bus then
            None
        else

        let lMajor = b.[2]
        let lMinor = b.[3]
        let codeSize = toUint32 (b.Slice (4, 4))
        let initialisedDataSize = toUint32 (b.Slice (8, 4))
        let uninitialisedDataSize = toUint32 (b.Slice (12, 4))
        let entryPointRva = toUint32 (b.Slice (16, 4))
        let baseOfCode = toUint32 (b.Slice (20, 4))
        let baseOfData = toUint32 (b.Slice (24, 4))

        {
            CodeSize = codeSize
            InitialisedDataSize = initialisedDataSize
            UninitialisedDataSize = uninitialisedDataSize
            EntryPointRva = entryPointRva
            BaseOfCode = baseOfCode
            BaseOfData = baseOfData
            LMajor = lMajor
            LMinor = lMinor
        }
        |> Some

type WindowsSubsystem =
    | Cui
    | Gui

    static member Parse (b : byte) : WindowsSubsystem option =
        if b = 3uy then WindowsSubsystem.Cui |> Some
        elif b = 2uy then WindowsSubsystem.Gui |> Some
        else None

type PeHeaderNtSpecificFields =
    {
        ImageBase : uint32
        SectionAlignment : uint32
        ImageSize : uint32
        HeaderSize : uint32
        WindowsSubsystem : WindowsSubsystem
    }

    static member Parse (b : ReadOnlySpan<byte>) : PeHeaderNtSpecificFields option =
        let imageBase = toUint32 (b.Slice (0, 4))
        let sectionAlignment = toUint32 (b.Slice (4, 4))
        let fileAlignment = toUint32 (b.Slice (8, 4))

        if sectionAlignment <= fileAlignment then
            None
        else if

            //if toUint16 (b.Slice (12, 2)) <> 5us then
            //    None
            toUint16 (b.Slice (14, 2)) <> 0us
        then
            None
        elif toUint16 (b.Slice (16, 2)) <> 0us then
            None
        elif toUint16 (b.Slice (18, 2)) <> 0us then
            None
        //elif toUint16 (b.Slice (20, 2)) <> 5us then
        //    None
        elif toUint16 (b.Slice (22, 2)) <> 0us then
            None
        elif toUint32 (b.Slice (24, 4)) <> 0u then
            None
        else

        let imageSize = toUint32 (b.Slice (28, 4))

        if imageSize % fileAlignment <> 0u then
            None
        else

        let headerSize = toUint32 (b.Slice (32, 4))

        if headerSize % fileAlignment <> 0u then
            None
        else if toUint32 (b.Slice (36, 4)) <> 0u then
            None
        else if b.[41] <> 0uy then
            None
        else

        match WindowsSubsystem.Parse b.[40] with
        | None -> None
        | Some windowsSubsystem ->

        //if toUint32 (b.Slice (42, 4)) <> 0x100000u then
        //    None
        //elif toUint32 (b.Slice (46, 4)) <> 0x1000u then
        //    None
        //elif toUint32 (b.Slice (52, 4)) <> 0x100000u then
        //    None
        //elif toUint32 (b.Slice (56, 4)) <> 0x1000u then
        //    None
        if toUint32 (b.Slice (60, 4)) <> 0u then
            None
        elif toUint32 (b.Slice (64, 4)) <> 0x10u then
            None
        else
        // TODO: DLL Flags, II.25.2.3.2

        {
            ImageBase = imageBase
            SectionAlignment = sectionAlignment
            ImageSize = imageSize
            HeaderSize = headerSize
            WindowsSubsystem = windowsSubsystem
        }
        |> Some

type SectionCharacteristics =
    {
        Code : bool
        Initialised : bool
        Uninitialised : bool
        ExecutedAsCode : bool
        Readable : bool
        Writable : bool
    }

    static member Parse (b : ReadOnlySpan<byte>) : SectionCharacteristics =
        assert (b.Length = 4)
        let code = b[0] &&& 0x20uy = 0x20uy
        let initialised = b[0] &&& 0x40uy = 0x40uy
        let uninitialised = b[0] &&& 0x80uy = 0x80uy
        let executable = b[3] &&& 0x20uy = 0x20uy
        let readable = b[3] &&& 0x40uy = 0x40uy
        let writable = b[3] &&& 0x80uy = 0x80uy

        {
            Code = code
            Initialised = initialised
            Uninitialised = uninitialised
            ExecutedAsCode = executable
            Readable = readable
            Writable = writable
        }

type SectionHeader =
    {
        Name : string
        VirtualSize : uint32
        VirtualAddress : uint32
        SizeOfRawData : uint32
        PointerToRawData : uint32
        Characteristics : SectionCharacteristics
    }

    static member Parse (b : ReadOnlySpan<byte>) : SectionHeader option =
        assert (b.Length = 40)
        let name = Encoding.ASCII.GetString (b.Slice (0, 8)) |> fun s -> s.TrimEnd (char 0)
        let virtualSize = toUint32 (b.Slice (8, 4))
        let virtualAddress = toUint32 (b.Slice (12, 4))
        let sizeOfRawData = toUint32 (b.Slice (16, 4))
        let pointerToRawData = toUint32 (b.Slice (20, 4))

        if toUint32 (b.Slice (24, 4)) <> 0u then
            None
        elif toUint32 (b.Slice (28, 4)) <> 0u then
            None
        elif toUint16 (b.Slice (32, 2)) <> 0us then
            None
        elif toUint16 (b.Slice (34, 2)) <> 0us then
            None
        else

        let characteristics = SectionCharacteristics.Parse (b.Slice (36, 4))

        {
            Name = name
            VirtualSize = virtualSize
            VirtualAddress = virtualAddress
            SizeOfRawData = sizeOfRawData
            PointerToRawData = pointerToRawData
            Characteristics = characteristics
        }
        |> Some

type RvaAndSize =
    {
        Rva : uint32
        BlockSize : uint32
    }

    static member Parse (b : ReadOnlySpan<byte>) : RvaAndSize =
        {
            Rva = toUint32 (b.Slice (0, 4))
            BlockSize = toUint32 (b.Slice (4, 4))
        }

type CliRuntimeFlags =
    {
        Requires32Bit : bool
        HasStrongNameSig : bool
    }

    static member Parse (b : ReadOnlySpan<byte>) : CliRuntimeFlags option =
        if b.[0] &&& 1uy <> 1uy then
            None
        elif b.[0] &&& 0x10uy <> 0x10uy then
            None
        elif b.[2] &&& 1uy <> 1uy then
            None
        else

        {
            Requires32Bit = b.[0] &&& 2uy = 2uy
            HasStrongNameSig = b.[0] &&& 8uy = 8uy
        }
        |> Some

type CliHeader =
    {
        SizeInBytes : uint32
        MajorRuntimeMinVersion : uint16
        MinorRuntimeMinVersion : uint16
        Metadata : RvaAndSize
        Flags : CliRuntimeFlags
        EntryPointToken : unit
        Resources : RvaAndSize
        StrongNameSignature : RvaAndSize
        VTableFixups : RvaAndSize
    }

    static member Parse (b : ReadOnlySpan<byte>) : CliHeader option =
        let sizeInBytes = toUint32 (b.Slice (0, 2))
        let majorVersion = toUint16 (b.Slice (4, 2))
        let minorVersion = toUint16 (b.Slice (6, 2))
        let metadata = RvaAndSize.Parse (b.Slice (8, 8))

        match CliRuntimeFlags.Parse (b.Slice (16, 4)) with
        | None -> None
        | Some flags ->

        let entryPointToken = () //(b.Slice (20, 4))
        let resources = RvaAndSize.Parse (b.Slice (24, 8))
        let strongNameSignature = RvaAndSize.Parse (b.Slice (32, 8))

        if toUint64 (b.Slice (40, 8)) <> 0UL then
            None
        else

        let vTableFixups = RvaAndSize.Parse (b.Slice (48, 8))

        if toUint64 (b.Slice (56, 8)) <> 0UL then
            None
        elif toUint64 (b.Slice (64, 8)) <> 0UL then
            None
        else

        {
            SizeInBytes = sizeInBytes
            MajorRuntimeMinVersion = majorVersion
            MinorRuntimeMinVersion = minorVersion
            Metadata = metadata
            Flags = flags
            EntryPointToken = entryPointToken
            Resources = resources
            StrongNameSignature = strongNameSignature
            VTableFixups = vTableFixups
        }
        |> Some

type DataDirectories =
    {
        ImportTable : RvaAndSize
        RelocationTable : RvaAndSize option
        ImportAddressTable : RvaAndSize
        CliHeader : RvaAndSize
    }

    static member Parse (b : ReadOnlySpan<byte>) : DataDirectories option =
        // Ignore the export table
        // if toUint64 (b.Slice (0, 8)) <> 0UL then
        //     None
        // else
        let importTable = RvaAndSize.Parse (b.Slice (8, 8))
        // Ignore the resource table, exception table, certificate table
        // if toUint64 (b.Slice (16, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (24, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (32, 8)) <> 0UL then
        //     None
        // else
        let relocationTable =
            if toUint64 (b.Slice (40, 8)) = 0UL then
                None
            else
                Some (RvaAndSize.Parse (b.Slice (40, 8)))
        // Ignore the debug, copyright, global ptr, tls table, laod config table, bound import
        // if toUint64 (b.Slice (48, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (56, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (64, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (72, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (80, 8)) <> 0UL then
        //     None
        // elif toUint64 (b.Slice (88, 8)) <> 0UL then
        //     None
        // else
        let iat = RvaAndSize.Parse (b.Slice (96, 8))
        // Ignore the delay import descriptor
        // if toUint64 (b.Slice (104, 8)) <> 0UL then
        //     None
        // else
        let cliHeader = RvaAndSize.Parse (b.Slice (112, 8))

        if toUint64 (b.Slice (120, 8)) <> 0UL then
            None
        else

        {
            ImportTable = importTable
            RelocationTable = relocationTable
            ImportAddressTable = iat
            CliHeader = cliHeader
        }
        |> Some

type PeOptionalHeader =
    {
        StandardFields : PeHeaderStandardFields
        NtSpecificFields : PeHeaderNtSpecificFields
        DataDirectories : DataDirectories
    }

    static member Parse (b : ReadOnlySpan<byte>) : PeOptionalHeader option =
        match PeHeaderStandardFields.Parse (b.Slice (0, 28)) with
        | None -> None
        | Some standard ->

        match PeHeaderNtSpecificFields.Parse (b.Slice (28, 68)) with
        | None -> None
        | Some nt ->

        match DataDirectories.Parse (b.Slice (96, 128)) with
        | None -> None
        | Some dd ->

        {
            StandardFields = standard
            NtSpecificFields = nt
            DataDirectories = dd
        }
        |> Some

type MsAssembly =
    {
        PEOffset : uint32
        NumberOfSections : uint16
        CreationDate : DateTime
        OptionalHeaderSize : uint16
        Characteristics : Characteristics
        OptionalHeader : PeOptionalHeader
    }

[<RequireQualifiedAccess>]
module MsAssembly =

    let private msdosHeader1 : byte[] =
        [|
            0x4d
            0x5a
            0x90
            0
            3
            0
            0
            0
            4
            0
            0
            0
            0xff
            0xff
            0
            0
            0xb8
            0
            0
            0
            0
            0
            0
            0
            0x40
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
            0
        |]
        |> Array.map byte

    let private msdosHeader2 : byte[] =
        [|
            0xe
            0x1f
            0xba
            0x0e
            0
            0xb4
            9
            0xcd
            0x21
            0xb8
            1
            0x4c
            0xcd
            0x21
            0x54
            0x68
            0x69
            0x73
            0x20
            0x70
            0x72
            0x6f
            0x67
            0x72
            0x61
            0x6d
            0x20
            0x63
            0x61
            0x6e
            0x6e
            0x6f
            0x74
            0x20
            0x62
            0x65
            0x20
            0x72
            0x75
            0x6e
            0x20
            0x69
            0x6e
            0x20
            0x44
            0x4f
            0x53
            0x20
            0x6d
            0x6f
            0x64
            0x65
            0x2e
            0x0d
            0x0d
            0x0a
            0x24
            0x00
            0x00
            0x00
            0x00
            0x00
            0x00
            0x00
        |]
        |> Array.map byte

    let parse (bytes : byte[]) : MsAssembly option =
        let bytes : ReadOnlySpan<byte> = Span.op_Implicit (bytes.AsSpan ())

        if not (MemoryExtensions.SequenceEqual (bytes.Slice (0, 60), msdosHeader1)) then
            None
        else

        let peOffset = toUint32 (bytes.Slice (60, 4))

        if not (MemoryExtensions.SequenceEqual (bytes.Slice (64, 64), msdosHeader2)) then
            None
        else if

            not (MemoryExtensions.SequenceEqual (bytes.Slice (int peOffset, 2), "PE"B))
        then
            None
        else if

            not (MemoryExtensions.SequenceEqual (bytes.Slice (int peOffset + 2, 2), [| 0uy ; 0uy |]))
        then
            None
        else

        let peOffset = peOffset + 4u

        let numberOfSections = toUint16 (bytes.Slice (int (peOffset + 2u), 2))

        let creationDate =
            DateTime.UnixEpoch.AddSeconds (toUint32 (bytes.Slice (int (peOffset + 4u), 4)) |> float)

        if
            not (
                MemoryExtensions.SequenceEqual (
                    bytes.Slice (int peOffset + 8, 8),
                    [| 0uy ; 0uy ; 0uy ; 0uy ; 0uy ; 0uy ; 0uy ; 0uy |]
                )
            )
        then
            None
        else

        let optionalHeaderSize = toUint16 (bytes.Slice (int (peOffset + 16u), 2))

        match Characteristics.Parse (bytes.Slice (int (peOffset + 18u), 2)) with
        | None -> None
        | Some characteristics ->

        match PeOptionalHeader.Parse (bytes.Slice (int (peOffset + 20u), int optionalHeaderSize)) with
        | None -> None
        | Some optionalHeader ->

        {
            PEOffset = peOffset - 4u
            NumberOfSections = numberOfSections
            CreationDate = creationDate
            Characteristics = characteristics
            OptionalHeaderSize = optionalHeaderSize
            OptionalHeader = optionalHeader
        }
        |> Some
