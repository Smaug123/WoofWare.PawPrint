{
  description = "A .NET runtime.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pname = "WoofWare.PawPrint";
      dotnet-sdk = pkgs.dotnetCorePackages.sdk_10_0;
      dotnet-runtime = pkgs.dotnetCorePackages.runtime_10_0;
      version = "0.6";
      # The .NET servicing version we emulate. This must equal what nixpkgs provides (enforced by
      # the `runtime-version-pin` check below) and WoofWare.PawPrint/EmulatedRuntime.fs (enforced by
      # the TestEmulatedRuntime drift test). When nixpkgs bumps the SDK, bump all of these together.
      expectedRuntimeVersion = "10.0.7";
      # Pinned, read-only dotnet/runtime source, for checking upstream behaviour (BCL / QCall /
      # native helpers) from the devshell as $DOTNET_RUNTIME_SRC. Sparse-checked-out to just the
      # trees we actually read, to keep the closure small.
      #
      # Pin `rev` to the commit the public `v${expectedRuntimeVersion}` tag resolves to — NOT the
      # binary's internal `.version` / `dotnet --info` build commit, which is frequently not pushed
      # to the public repo and so cannot be fetched.
      #
      # Heads-up on a dotnet quirk: the release commit is tagged BEFORE the in-tree version is
      # bumped, so this tree's `eng/Versions.props` self-reports `<PatchVersion>6` even though it is
      # the v10.0.7 release source. The tag is the release identity; the in-tree version lags by one
      # (confirmed: v10.0.5 -> 4, v10.0.7 -> 6, v10.0.8 -> 7). Pin by the tag, not the in-tree string.
      dotnet-runtime-src = pkgs.fetchgit {
        url = "https://github.com/dotnet/runtime";
        rev = "7706f546bac1a99b3d891afe3591dc88c67f0cc4"; # v10.0.7 (tree self-reports 10.0.6; see above)
        hash = "sha256-vDIXH6/gQMh5xQI5WSG+HhJ6La44QsQBgRjGq4XyPjc=";
        sparseCheckout = [
          "src/coreclr"
          "src/libraries/System.Private.CoreLib"
          # hostfxr/hostpolicy: the native host that reads `runtimeconfig.json` and hands
          # the resulting properties to `AppContext.Setup`. `runtime_config.cpp` is the
          # authority on how a JSON value becomes the string a guest sees, and it defers
          # to the vendored rapidjson below for every non-string value.
          "src/native/corehost"
          "src/native/external/rapidjson"
          # The `SystemNative_*` boundary, which `Native/NativeSystemNative.fs` reimplements.
          # Two halves, and we need both:
          #  - `Common/src/Interop/Unix` is the *managed* side: the `[LibraryImport]` declarations
          #    that say what CoreLib passes and expects back, the `FileStatus` layout `FStat` fills
          #    in, and `Interop.Errors.cs`'s `Error`/`ErrorInfo` PAL enum.
          #  - `native/libs/System.Native` is the C shim itself, which is the authority on the
          #    behaviour we have to reproduce: PAL-to-platform flag translation, EINTR retry loops,
          #    and which errno each failure reports. Handlers in `NativeSystemNative.fs` already
          #    cite `pal_console.c` / `pal_time.c` / `pal_runtimeinformation.c` by line.
          #  - `native/libs/Common` holds the headers those `.c` files defer to, and it is where
          #    the tables actually live: `pal_error_common.h` has the raw-errno-to-PAL-`Error`
          #    conversion that `SystemNative_ConvertErrorPlatformToPal` is a one-line wrapper
          #    around, and `pal_io_common.h` has the `Common_Read`/`Common_Write` bodies that
          #    `UnixError.fs` cites for their negative-size ERANGE contract.
          "src/libraries/Common/src/Interop/Unix"
          "src/native/libs/System.Native"
          "src/native/libs/Common"
          "eng"
        ];
      };
      # The target framework moniker the runtime pack lays its managed assemblies out under.
      # Tracks the major version in expectedRuntimeVersion; the install phase below fails loudly
      # if the pack's layout ever stops matching, rather than silently producing an empty dir.
      linuxFrameworkTfm = "net10.0";
      # The managed framework assemblies from the linux-x64 runtime pack, at the same servicing
      # version we emulate, exposed to the devshell as $DOTNET_LINUX_FRAMEWORK_DIR.
      #
      # Why this exists: PawPrint interprets whichever CoreLib it is pointed at, and CoreLib is
      # `#if`-split per target — e.g. `Lock.ThreadId.InitializeForCurrentThread` calls
      # `GetUInt64OSThreadId` under TARGET_OSX and `TryGetUInt32OSThreadId` elsewhere. Every
      # entry point otherwise resolves the *host's* shared framework, so a macOS dev box can
      # only ever exercise the macOS BCL while production and CI run the Linux one. Pointing the
      # interpreter's runtime-dir list at this pack closes that gap.
      #
      # Only the managed assemblies are kept: PawPrint never loads native code (that is the
      # point of the project), so the pack's `native/` tree is closure for nothing.
      #
      # The version tracks expectedRuntimeVersion, so `hash` must be bumped in the same commit.
      dotnet-linux-framework = pkgs.stdenvNoCC.mkDerivation {
        pname = "dotnet-linux-x64-framework";
        version = expectedRuntimeVersion;
        src = pkgs.fetchurl {
          url = "https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.linux-x64/${expectedRuntimeVersion}/microsoft.netcore.app.runtime.linux-x64.${expectedRuntimeVersion}.nupkg";
          hash = "sha256-0IUm9tRbSam4/WKnyawtfKs/q1pHOxxJ4AUOAPJSYvo=";
        };
        nativeBuildInputs = [pkgs.unzip];
        # A .nupkg is a zip, but the default unpackPhase dispatches on file extension and does
        # not recognise it.
        unpackPhase = ''
          runHook preUnpack
          unzip -qq "$src" -d pack
          runHook postUnpack
        '';
        installPhase = ''
          runHook preInstall
          libDir="pack/runtimes/linux-x64/lib/${linuxFrameworkTfm}"
          if [ ! -f "$libDir/System.Private.CoreLib.dll" ]; then
            echo "Runtime pack layout changed: expected $libDir/System.Private.CoreLib.dll." >&2
            echo "Check linuxFrameworkTfm against the pack contents." >&2
            exit 1
          fi
          mkdir -p "$out"
          cp "$libDir"/*.dll "$out/"
          runHook postInstall
        '';
        # Managed assemblies: nothing to strip, patchelf, or rewrite. Fixup on 170-odd DLLs is
        # pure cost, and on macOS the darwin fixup hooks would inspect them pointlessly.
        dontFixup = true;
      };
      dotnetTool = dllOverride: toolName: toolVersion: hash:
        pkgs.stdenvNoCC.mkDerivation rec {
          name = toolName;
          version = toolVersion;
          nativeBuildInputs = [pkgs.makeWrapper];
          src = pkgs.fetchNuGet {
            pname = name;
            version = version;
            hash = hash;
            installPhase = ''mkdir -p $out/bin && cp -r tools/net*/any/* $out/bin'';
          };
          installPhase = let
            dll =
              if isNull dllOverride
              then name
              else dllOverride;
          in
            # fsharp-analyzers requires the .NET SDK at runtime, so we use that instead of dotnet-runtime.
            ''
              runHook preInstall
              mkdir -p "$out/lib"
              cp -r ./bin/* "$out/lib"
              makeWrapper "${dotnet-sdk}/bin/dotnet" "$out/bin/${name}" --set DOTNET_HOST_PATH "${dotnet-sdk}/bin/dotnet" --add-flags "$out/lib/${dll}.dll"
              runHook postInstall
            '';
        };
    in {
      packages = let
        deps = builtins.fromJSON (builtins.readFile ./nix/deps.json);
      in {
        fantomas = dotnetTool null "fantomas" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fantomas.version (builtins.head (builtins.filter (elem: elem.pname == "fantomas") deps)).hash;
        fsharp-analyzers = dotnetTool "FSharp.Analyzers.Cli" "fsharp-analyzers" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fsharp-analyzers.version (builtins.head (builtins.filter (elem: elem.pname == "fsharp-analyzers") deps)).hash;
        default = pkgs.buildDotnetModule {
          inherit pname version dotnet-sdk dotnet-runtime;
          name = "WoofWare.PawPrint";
          src = ./.;
          projectFile = "./WoofWare.PawPrint/WoofWare.PawPrint.fsproj";
          testProjectFile = "./WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj";
          nugetDeps = ./nix/deps.json; # `nix build .#default.fetch-deps && ./result nix/deps.json`
          doCheck = true;
        };
      };
      checks = {
        # Fails the build (and so `nix flake check` in CI) when nixpkgs's runtime version drifts
        # away from the version we pin. Forces a deliberate bump of dotnet-runtime-src, of
        # expectedRuntimeVersion, and of WoofWare.PawPrint/EmulatedRuntime.fs in lockstep.
        runtime-version-pin =
          pkgs.runCommand "runtime-version-pin" {}
          (
            if dotnet-runtime.version == expectedRuntimeVersion
            then "touch $out"
            else ''
              echo "Runtime pin drift: nixpkgs provides ${dotnet-runtime.version} but expectedRuntimeVersion = ${expectedRuntimeVersion}." >&2
              echo "Bump dotnet-runtime-src rev (to the public v<new> tag commit), expectedRuntimeVersion, and WoofWare.PawPrint/EmulatedRuntime.fs together." >&2
              exit 1
            ''
          );
      };
      devShell = pkgs.mkShell {
        buildInputs = [dotnet-sdk];
        DOTNET_CLI_TELEMETRY_OPTOUT = "1";
        # Pinned read-only dotnet/runtime source for checking upstream behaviour; see the
        # sync-dotnet-runtime command. Replaces the old ad-hoc ../dotnet-runtime sibling checkout.
        DOTNET_RUNTIME_SRC = dotnet-runtime-src;
        # Managed linux-x64 framework assemblies, so tests can point the interpreter at the
        # CoreLib flavour production runs instead of the host's. See dotnet-linux-framework.
        # Tests that need it skip when it is unset, so a non-Nix checkout still works.
        DOTNET_LINUX_FRAMEWORK_DIR = dotnet-linux-framework;
        # Force polling-based file watcher to avoid hangs in
        # FileSystemWatcher.StartRaisingEvents on macOS (FSEvents/CoreFoundation
        # path can deadlock under load when ASP.NET hosts created in tests
        # initialise their JSON configuration providers).
        DOTNET_USE_POLLING_FILE_WATCHER = "1";
        packages = [
          pkgs.alejandra
          pkgs.lychee
          pkgs.shellcheck
          pkgs.xmlstarlet
          pkgs.claude-code
          pkgs.codex
        ];
      };
    });
}
