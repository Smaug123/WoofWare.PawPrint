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
  # flake-utils' default systems less x86_64-darwin, whose support nixpkgs dropped
  # in 26.11: from then on, evaluating anything for it fails.
    flake-utils.lib.eachSystem ["aarch64-darwin" "aarch64-linux" "x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pname = "WoofWare.PawPrint";
      version = "0.6";
      # Each runtime PawPrint emulates, and what the flake pins for it. The oldest one is the one
      # the test host and every executable run on, and it alone supplies the SDK; a newer one is
      # only ever a framework a guest runs on. See docs/plans/2026-09-10-multi-runtime-support.md.
      #
      # - `runtime` is nixpkgs's shared framework for that major.
      # - `expectedRuntimeVersion` is the version we emulate. It must equal `runtime.version`
      #   (enforced by the `runtime-version-pin-<name>` checks below). For each runtime
      #   `EmulatedRuntime` has a case for, the same build is pinned in
      #   WoofWare.PawPrint.Semantics/EmulatedRuntime.fs (`EmulatedRuntime.pin`), as the build identity the
      #   CoreLib's AssemblyInformationalVersion states (e.g. "10.0.12-servicing.26422.108"); the
      #   TestEmulatedRuntime drift test compares that against the CoreLib the suite loads. When
      #   nixpkgs bumps a runtime, bump all of that runtime's pins together.
      # - `sourceRev` and `sourceHash` pin its dotnet/runtime source (`runtimeSource` below).
      #   Pin `sourceRev` to the commit the public `v${expectedRuntimeVersion}` tag resolves to,
      #   NOT the binary's internal `.version` / `dotnet --info` build commit, which is frequently
      #   not pushed to the public repo and so cannot be fetched.
      #
      #   Heads-up on a dotnet quirk: a tagged tree's `eng/Versions.props` need not state the
      #   tag's patch version. Some release commits are tagged before the in-tree version is
      #   bumped, and self-report the previous patch (v10.0.5 -> 4, v10.0.7 -> 6, v10.0.8 -> 7);
      #   others agree (v10.0.12 -> 12). The tag is the release identity: pin by it, not by the
      #   in-tree string.
      # - `frameworkPackTfm`, `linuxFrameworkHash` and `osxFrameworkHash` describe its linux-x64
      #   and osx-arm64 runtime packs (`frameworkPack` below). The TFM tracks the major version;
      #   each hash tracks `expectedRuntimeVersion`, so bump both in the same commit.
      # - `aspNetCore`, on a runtime newer than the oldest, is nixpkgs's ASP.NET Core shared
      #   framework at that runtime's version, which guests that run ASP.NET Core bind. The oldest
      #   runtime's comes with the SDK. Its version must equal `expectedRuntimeVersion` too.
      runtimes = {
        net10 = {
          runtime = pkgs.dotnetCorePackages.runtime_10_0;
          expectedRuntimeVersion = "10.0.12";
          sourceRev = "4271d88e0aebf3d04f188f1334c2220d80555ef6"; # v10.0.12
          sourceHash = "sha256-rSa1KZND+LRcz3iVLq0rzZx4KBmxFJ5tkj+Ums3cOOM=";
          frameworkPackTfm = "net10.0";
          linuxFrameworkHash = "sha256-iGni+5dLyBD/5AuPuOGLf/sdGgPlKglMgEZoxNeTsa4=";
          osxFrameworkHash = "sha256-Lw5y/feerZHQ4a1JYOo6m966G3ywhGfj6tCkJ9H9Lgg=";
        };
        net11 = {
          runtime = pkgs.dotnetCorePackages.runtime_11_0;
          expectedRuntimeVersion = "11.0.0-rc.1.26425.128";
          sourceRev = "ab19415702aa8139d5369e47c73edb47343c34ad"; # v11.0.0-rc.1.26425.128
          sourceHash = "sha256-q9OZEYDuXnbTdi81CDsObuL+DyyOioRi7RqHIxYTjoA=";
          frameworkPackTfm = "net11.0";
          linuxFrameworkHash = "sha256-+oYmT/VOyfRO84M5ZnmQerKBlPSbXf4DIT8VoqGSJLY=";
          osxFrameworkHash = "sha256-DNGYmohVcVwXZv4oJIhqiRC/jFhJdCORLIvJ1VrCIsM=";
          aspNetCore = pkgs.dotnetCorePackages.aspnetcore_11_0;
        };
      };
      # The oldest runtime, which the test host, the package and every executable run on.
      dotnet-sdk = pkgs.dotnetCorePackages.sdk_10_0;
      dotnet-runtime = runtimes.net10.runtime;
      # Self-contained publishing targets, matching the flake's supported platforms.
      publishRids = ["linux-x64" "linux-arm64" "osx-arm64"];
      # The devshell's one `dotnet`: the oldest runtime's SDK, with every newer runtime's shared
      # frameworks beside it, so the same muxer runs the test host and a guest on any runtime. It
      # resolves the newest `hostfxr` present, so net10 apps start through net11's; `hostfxr` is
      # the version-agnostic half of the host, which only selects a framework directory.
      dotnet-devshell = pkgs.dotnetCorePackages.combinePackages [dotnet-sdk runtimes.net11.runtime runtimes.net11.aspNetCore];
      # The Nix build's pinned packages and SDK packs for every supported publishing target.
      # NuGet can read these directly as a fallback folder without extracting them into
      # a writable cache. The source contains the metadata needed for version resolution.
      offlineNuget = pkgs.symlinkJoin {
        name = "pawprint-offline-nuget";
        paths =
          [
            (pkgs.mkNugetDeps {
              name = pname;
              sourceFile = ./nix/deps.json;
            })
          ]
          ++ dotnet-sdk.packages
          ++ pkgs.lib.concatMap (rid: dotnet-sdk.targetPackages.${rid}) publishRids;
      };
      # The SDK's local-tool downloader needs full archives and an explicit NuGet config;
      # it does not use MSBuild's RestoreSources or the fallback folder. Only manifest
      # tools need installable archives; project packages are read directly from the store.
      offlineToolNuget = pkgs.mkNugetDeps {
        name = "${pname}-tools";
        installable = true;
        nugetDeps = {fetchNuGet}: let
          localTools = (pkgs.lib.importJSON ./.config/dotnet-tools.json).tools;
          isLocalTool = dep:
            builtins.any
            (name: pkgs.lib.toLower name == pkgs.lib.toLower dep.pname && localTools.${name}.version == dep.version)
            (builtins.attrNames localTools);
        in
          map fetchNuGet (builtins.filter isLocalTool (pkgs.lib.importJSON ./nix/deps.json));
      };
      offlineNugetEnvironment = {
        RestoreSources = "${offlineNuget}/share/nuget/source";
        NUGET_FALLBACK_PACKAGES = "${offlineNuget}/share/nuget/packages";
        PAWPRINT_TOOL_NUGET_CONFIG = pkgs.writeText "pawprint-tool-nuget.config" ''
          <?xml version="1.0" encoding="utf-8"?>
          <configuration>
            <packageSources>
              <clear />
              <add key="offline-tools" value="${offlineToolNuget}/share/nuget/source" />
            </packageSources>
          </configuration>
        '';
        # Auditing uses an online advisory feed independently of package resolution.
        NuGetAudit = "false";
      };
      # Pinned, read-only dotnet/runtime source, for checking upstream behaviour (BCL / QCall /
      # native helpers) from the devshell as $DOTNET_RUNTIME_SRC_<RUNTIME>. Sparse-checked-out to
      # just the trees we actually read, to keep the closure small.
      runtimeSource = pin:
        pkgs.fetchgit {
          url = "https://github.com/dotnet/runtime";
          rev = pin.sourceRev;
          hash = pin.sourceHash;
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
            #  - `native/minipal` is where several of those `.c` files keep their *entire* body:
            #    `SystemNative_GetProcessPath` is `return minipal_getexepath();` and nothing else,
            #    so `getexepath.h` is the only statement of which errno each failure reports.
            "src/libraries/Common/src/Interop/Unix"
            "src/native/libs/System.Native"
            "src/native/libs/Common"
            "src/native/minipal"
            "eng"
          ];
        };
      # The managed framework assemblies from one of a runtime's runtime packs (`rid` is
      # linux-x64 or osx-arm64, the two flavours the emulated kernel models), at the version we
      # emulate, exposed to the devshell as $DOTNET_LINUX_FRAMEWORK_DIR_<RUNTIME> and
      # $DOTNET_OSX_FRAMEWORK_DIR_<RUNTIME>.
      #
      # Why this exists: PawPrint interprets whichever CoreLib it is pointed at, and CoreLib is
      # `#if`-split per target — e.g. `Lock.ThreadId.InitializeForCurrentThread` calls
      # `GetUInt64OSThreadId` under TARGET_OSX and `TryGetUInt32OSThreadId` elsewhere. Every
      # entry point otherwise resolves the *host's* shared framework, so a macOS dev box can
      # only ever exercise the macOS BCL while production and CI run the Linux one, and a guest
      # on an emulated kernel of the other flavour would run a CoreLib that kernel never meets.
      # Pointing the interpreter's runtime-dir list at the pack of the kernel's flavour closes
      # that gap.
      #
      # Only the managed assemblies are kept: PawPrint never loads native code (that is the
      # point of the project), so the pack's `native/` tree is closure for nothing. The install
      # phase fails loudly if the pack's layout ever stops matching `frameworkPackTfm`, rather
      # than silently producing an empty dir.
      frameworkPack = rid: hash: name: pin:
        pkgs.stdenvNoCC.mkDerivation {
          pname = "dotnet-${rid}-framework";
          version = pin.expectedRuntimeVersion;
          src = pkgs.fetchurl {
            url = "https://api.nuget.org/v3-flatcontainer/microsoft.netcore.app.runtime.${rid}/${pin.expectedRuntimeVersion}/microsoft.netcore.app.runtime.${rid}.${pin.expectedRuntimeVersion}.nupkg";
            inherit hash;
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
            libDir="pack/runtimes/${rid}/lib/${pin.frameworkPackTfm}"
            if [ ! -f "$libDir/System.Private.CoreLib.dll" ]; then
              echo "Runtime pack layout changed: expected $libDir/System.Private.CoreLib.dll." >&2
              echo "Check frameworkPackTfm for ${name} against the pack contents." >&2
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
      # `scripts/test_footprint.py` needs hypothesis; the devshell and the `footprint` check share it.
      python = pkgs.python3.withPackages (ps: [ps.hypothesis]);
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
        nuget-deps = offlineNuget;
        fantomas = dotnetTool null "fantomas" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fantomas.version (builtins.head (builtins.filter (elem: elem.pname == "fantomas") deps)).hash;
        fsharp-analyzers = dotnetTool "FSharp.Analyzers.Cli" "fsharp-analyzers" (builtins.fromJSON (builtins.readFile ./.config/dotnet-tools.json)).tools.fsharp-analyzers.version (builtins.head (builtins.filter (elem: elem.pname == "fsharp-analyzers") deps)).hash;
        default = pkgs.buildDotnetModule {
          inherit pname version dotnet-sdk dotnet-runtime;
          name = "WoofWare.PawPrint";
          src = ./.;
          projectFile = "./WoofWare.PawPrint/WoofWare.PawPrint.fsproj";
          # Restore/build the whole solution so fetch-deps also captures development-only
          # dependencies (including BenchmarkDotNet) for the offline devshell.
          testProjectFile = "./WoofWare.PawPrint.slnx";
          nugetDeps = ./nix/deps.json; # `nix build .#default.fetch-deps && ./result nix/deps.json`
          # CI's `build` job runs this suite on every supported runtime, and running it here as
          # well would cost a CI runner another 10-20 minutes.
          doCheck = false;
        };
      };
      checks =
        {
          # Exercise project/tool restores and self-contained publishing with empty caches.
          # The Nix sandbox makes a missing package an error, even on a connected CI runner.
          offline-restore = pkgs.stdenvNoCC.mkDerivation {
            name = "pawprint-offline-restore";
            src = ./.;
            nativeBuildInputs = [dotnet-sdk];
            env = offlineNugetEnvironment;
            phases = ["unpackPhase" "installPhase"];
            installPhase = ''
              export NUGET_PACKAGES="$TMPDIR/packages"
              export DOTNET_CLI_HOME="$TMPDIR/dotnet-cli"
              export NUGET_HTTP_CACHE_PATH="$TMPDIR/nuget-http"
              dotnet restore WoofWare.PawPrint.slnx --force --no-http-cache
              for rid in ${pkgs.lib.escapeShellArgs publishRids}; do
                dotnet publish CSharpExample/CSharpExample.csproj --self-contained --configuration Release --runtime "$rid"
              done
              dotnet tool restore --configfile "$PAWPRINT_TOOL_NUGET_CONFIG" --no-http-cache
              dotnet fantomas --version
              dotnet fsharp-analyzers --help
              touch "$out"
            '';
          };
          # The docstring-attachment check is the oracle a definition-moving commit is held
          # to, and it is only as good as its own contract: thirty-six shapes in a throwaway
          # repository, eighteen of which must be reported and eighteen of which must not. The
          # wrapper CI runs has a contract of its own, that it never passes without having
          # compared anything. Runs here so that a change to either cannot quietly retire it.
          docstring-attachment =
            pkgs.runCommand "docstring-attachment" {
              buildInputs = [pkgs.python3 pkgs.git pkgs.bash];
            }
            ''
              export HOME=$TMPDIR
              ${pkgs.bash}/bin/bash ${./scripts/test-docstring-attachment.sh} \
                ${./scripts/check-docstring-attachment.py}
              ${pkgs.bash}/bin/bash ${./scripts/test-docstring-attachment-branch.sh} \
                ${./scripts/check-docstring-attachment-branch.sh} \
                ${./scripts/check-docstring-attachment.py}
              touch $out
            '';
          # scripts/footprint.py decides which commits move each package's version and which pull
          # requests run the tests, and its contract holds it to git's own sparse checkout and diff.
          # Runs here so that a change to the script cannot quietly retire it.
          footprint =
            pkgs.runCommand "footprint" {
              buildInputs = [python pkgs.git];
            }
            ''
              export HOME=$TMPDIR
              python3 ${./scripts/test_footprint.py} ${./scripts/footprint.py}
              touch $out
            '';
        }
        # One `runtime-version-pin-<name>` per runtime. Each fails the build (and so `nix flake
        # check` in CI) when nixpkgs's version of that runtime drifts away from the version we pin.
        # Forces a deliberate bump of that runtime's `sourceRev` and `sourceHash`,
        # `expectedRuntimeVersion`, `linuxFrameworkHash`, `osxFrameworkHash`, and (for a runtime `EmulatedRuntime` has a
        # case for) `EmulatedRuntime.pin` in WoofWare.PawPrint.Semantics/EmulatedRuntime.fs in lockstep.
        // pkgs.lib.mapAttrs' (name: pin:
          pkgs.lib.nameValuePair "runtime-version-pin-${name}"
          (pkgs.runCommand "runtime-version-pin-${name}" {}
            (
              if pin.runtime.version != pin.expectedRuntimeVersion
              then ''
                echo "Runtime pin drift for ${name}: nixpkgs provides ${pin.runtime.version} but expectedRuntimeVersion = ${pin.expectedRuntimeVersion}." >&2
                echo "Bump ${name}'s sourceRev (to the public v<new> tag commit), sourceHash, expectedRuntimeVersion, linuxFrameworkHash and osxFrameworkHash, and EmulatedRuntime.pin in WoofWare.PawPrint.Semantics/EmulatedRuntime.fs, together." >&2
                exit 1
              ''
              else if pin ? aspNetCore && pin.aspNetCore.version != pin.expectedRuntimeVersion
              then ''
                echo "ASP.NET Core pin drift for ${name}: nixpkgs provides ${pin.aspNetCore.version} but expectedRuntimeVersion = ${pin.expectedRuntimeVersion}." >&2
                exit 1
              ''
              else "touch $out"
            )))
        runtimes;
      devShells = let
        # What both shells share. NuGet restores from its usual sources.
        shell = {
          buildInputs = [dotnet-devshell];
          DOTNET_CLI_TELEMETRY_OPTOUT = "1";
          # Each runtime's variables carry its name, and no unsuffixed name exists, so nothing can
          # read one runtime's pins believing them another's. Tests read them only through
          # `FrameworkUnderTest` (TestFrameworkLocatorRatchet enforces it), keyed on the runtime.
          #
          # Pinned read-only dotnet/runtime source for checking upstream behaviour; see the
          # sync-dotnet-runtime skill.
          DOTNET_RUNTIME_SRC_NET10 = runtimeSource runtimes.net10;
          DOTNET_RUNTIME_SRC_NET11 = runtimeSource runtimes.net11;
          # Managed linux-x64 and osx-arm64 framework assemblies, so tests can point the
          # interpreter at the CoreLib flavour of the kernel it emulates instead of the host's.
          # See `frameworkPack`. Tests that need one skip when it is unset, so a non-Nix checkout
          # still works.
          DOTNET_LINUX_FRAMEWORK_DIR_NET10 = frameworkPack "linux-x64" runtimes.net10.linuxFrameworkHash "net10" runtimes.net10;
          DOTNET_LINUX_FRAMEWORK_DIR_NET11 = frameworkPack "linux-x64" runtimes.net11.linuxFrameworkHash "net11" runtimes.net11;
          DOTNET_OSX_FRAMEWORK_DIR_NET10 = frameworkPack "osx-arm64" runtimes.net10.osxFrameworkHash "net10" runtimes.net10;
          DOTNET_OSX_FRAMEWORK_DIR_NET11 = frameworkPack "osx-arm64" runtimes.net11.osxFrameworkHash "net11" runtimes.net11;
          # The net11 shared framework inside `dotnet-devshell`, as `dotnet --list-runtimes` names
          # it. The net10 one needs no variable: the test host runs on it, and finds it from its
          # own runtimeconfig.
          DOTNET_FRAMEWORK_DIR_NET11 = "${dotnet-devshell.unwrapped}/share/dotnet/shared/Microsoft.NETCore.App/${runtimes.net11.expectedRuntimeVersion}";
          # Likewise its ASP.NET Core shared framework, which net10's comes with the SDK.
          DOTNET_ASPNETCORE_FRAMEWORK_DIR_NET11 = "${dotnet-devshell.unwrapped}/share/dotnet/shared/Microsoft.AspNetCore.App/${runtimes.net11.expectedRuntimeVersion}";
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
            # The `*.py` probes under `docs/plans/` are run by hand from this shell, and
            # `scripts/check-docstring-attachment-branch.sh` runs its checker with it.
            python
          ];
        };
      in {
        default = pkgs.mkShell shell;
        # For a machine or sandbox with no network: NuGet restores only from the pinned Nix
        # closure (see README.md, "Building and testing offline").
        offline = pkgs.mkShell (shell
          // {
            env = offlineNugetEnvironment;
            shellHook = ''
              # Keep NuGet's writable bookkeeping within the checkout, which a sandbox can write.
              # Packages themselves resolve from the read-only store through the fallback folder.
              export NUGET_PACKAGES="$(git rev-parse --show-toplevel 2>/dev/null || pwd)/packages"
            '';
          });
      };
    });
}
