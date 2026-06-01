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
      version = "0.1";
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
        hash = "sha256-eMV1mZ2iy84CiHTOU2vZ5LaDFFAAyGlhetDKmBn0IMs=";
        sparseCheckout = [
          "src/coreclr"
          "src/libraries/System.Private.CoreLib"
          "eng"
        ];
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
