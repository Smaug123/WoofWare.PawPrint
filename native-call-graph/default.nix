# The call graphs of the C++ that WoofWare.PawPrint.Semantics/NativeContractTable.tsv cites, one
# per pinned runtime and Linux target, and the checks that hold the checked-in graphs to them.
# See README.md.
{
  pkgs,
  # nixpkgs for each target, whose glibc and GCC make its sysroot. Only their outputs are used,
  # substituted from the binary cache, so any host can build any target's graph.
  linuxPackages,
  # flake.nix's runtimes, and the fetcher of a runtime's pinned source.
  runtimes,
  runtimeSource,
  contractTable,
  contractCallees,
  # Where the checked-in graphs live.
  graphDirectory,
}: let
  inherit (pkgs) lib;
  llvm = pkgs.llvmPackages_21;
  clangMajor = lib.versions.major llvm.clang-unwrapped.version;
  resourceDir = "${llvm.clang-unwrapped.lib}/lib/clang/${clangMajor}";

  tool = pkgs.stdenv.mkDerivation {
    pname = "native-call-graph";
    version = "1";
    src = lib.fileset.toSource {
      root = ./.;
      fileset = lib.fileset.unions [./CMakeLists.txt ./callgraph.cpp];
    };
    nativeBuildInputs = [pkgs.cmake pkgs.ninja];
    buildInputs = [llvm.libclang llvm.llvm];
    cmakeFlags = ["-DCLANG_RESOURCE_DIR=${resourceDir}"];
  };

  targets = {
    linux-x64 = {
      packages = linuxPackages.linux-x64;
      clrArch = "x64";
      processor = "x86_64";
      multiarch = "x86_64-linux-gnu";
    };
    linux-arm64 = {
      packages = linuxPackages.linux-arm64;
      clrArch = "arm64";
      processor = "aarch64";
      multiarch = "aarch64-linux-gnu";
    };
  };

  # A Debian-shaped sysroot, so that CoreCLR's configure finds a target's libraries where its
  # own cross-build rootfs keeps them. Headers are copied, links resolved, so that every header
  # a unit reads is under the sysroot; libraries are linked.
  sysroot = name: target: let
    p = target.packages;
    gcc = p.gcc-unwrapped;
    gccTriple = p.stdenv.hostPlatform.config;
  in
    pkgs.runCommand "native-call-graph-sysroot-${name}" {
      passthru = {
        gccVersion = gcc.version;
        glibcVersion = p.glibc.version;
        inherit gccTriple;
      };
    } ''
      mkdir -p $out/usr/include $out/usr/lib/${target.multiarch} $out/usr/lib/gcc/${gccTriple}
      for headers in ${p.glibc.dev}/include ${p.lttng-ust.dev}/include ${p.liburcu.dev}/include; do
        cp -RL --no-preserve=mode $headers/. $out/usr/include/
      done
      mkdir -p $out/usr/include/c++
      cp -RL --no-preserve=mode ${gcc}/include/c++/${gcc.version} $out/usr/include/c++/${gcc.version}
      cp -RL --no-preserve=mode ${gcc}/lib/gcc/${gccTriple}/${gcc.version} $out/usr/lib/gcc/${gccTriple}/${gcc.version}
      for lib in ${p.glibc}/lib/* ${p.glibc.dev}/lib/* ${gcc.lib}/lib/*; do
        ln -sf $lib $out/usr/lib/${target.multiarch}/
      done
      # glibc's linker scripts name its libraries by store path, which a linker given a sysroot
      # looks for inside it.
      for script in $(grep -l 'GNU ld script' $out/usr/lib/${target.multiarch}/*.so); do
        sed -e 's|${p.glibc}/lib/|/usr/lib/${target.multiarch}/|g' -e 's|${p.glibc.dev}/lib/|/usr/lib/${target.multiarch}/|g' \
          $script > $script.rewritten
        rm $script
        mv $script.rewritten $script
      done
    '';

  toolchainFile = name: target: let
    root = sysroot name target;
    inherit (root) gccVersion gccTriple;
  in
    pkgs.writeText "native-call-graph-toolchain-${name}.cmake" ''
      set(CMAKE_SYSTEM_NAME Linux)
      set(CMAKE_SYSTEM_PROCESSOR ${target.processor})
      set(CMAKE_SYSROOT ${root})
      set(CMAKE_C_COMPILER ${llvm.clang-unwrapped}/bin/clang)
      set(CMAKE_CXX_COMPILER ${llvm.clang-unwrapped}/bin/clang++)
      set(CMAKE_C_COMPILER_TARGET ${target.multiarch})
      set(CMAKE_CXX_COMPILER_TARGET ${target.multiarch})
      set(CMAKE_AR ${llvm.llvm}/bin/llvm-ar)
      set(CMAKE_RANLIB ${llvm.llvm}/bin/llvm-ranlib)
      set(CMAKE_NM ${llvm.llvm}/bin/llvm-nm)
      set(CMAKE_OBJCOPY ${llvm.llvm}/bin/llvm-objcopy)
      set(CMAKE_OBJDUMP ${llvm.llvm}/bin/llvm-objdump)
      set(CMAKE_CXX_FLAGS_INIT "-nostdinc++ -isystem ${root}/usr/include/c++/${gccVersion} -isystem ${root}/usr/include/c++/${gccVersion}/${gccTriple}")
      set(CMAKE_EXE_LINKER_FLAGS_INIT "-fuse-ld=${llvm.lld}/bin/ld.lld -B${root}/usr/lib/gcc/${gccTriple}/${gccVersion} -L${root}/usr/lib/gcc/${gccTriple}/${gccVersion} -L${root}/usr/lib/${target.multiarch}")
      set(CMAKE_SHARED_LINKER_FLAGS_INIT "''${CMAKE_EXE_LINKER_FLAGS_INIT}")
      set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
      set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
      set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
    '';

  # What eng/versioning.targets writes before the native build, which the configure and the
  # units read; only the numbers matter, and they are the pin's.
  versionStubs = pin: let
    version = pin.expectedRuntimeVersion;
    parts = lib.splitString "." (builtins.head (lib.splitString "-" version));
    major = builtins.elemAt parts 0;
    minor = builtins.elemAt parts 1;
    patch = builtins.elemAt parts 2;
    header = pkgs.writeText "runtime_version.h" ''
      #define RuntimeAssemblyMajorVersion ${major}
      #define RuntimeAssemblyMinorVersion ${minor}
      #define RuntimeFileMajorVersion ${major}
      #define RuntimeFileMinorVersion ${minor}
      #define RuntimeFileBuildVersion 0
      #define RuntimeFileRevisionVersion 0
      #define RuntimeProductMajorVersion ${major}
      #define RuntimeProductMinorVersion ${minor}
      #define RuntimeProductPatchVersion ${patch}
      #define RuntimeProductVersion ${version}
    '';
  in
    pkgs.runCommand "native-call-graph-version-stubs" {} ''
      mkdir -p $out
      cp ${header} $out/runtime_version.h
      cp ${header} $out/_version.h
      echo 'static char sccsid[] __attribute__((used)) = "@(#)Version ${version}";' > $out/_version.c
    '';

  configureFlags = target: [
    "-DCMAKE_BUILD_TYPE=Release"
    "-DCLR_CMAKE_TARGET_ARCH=${target.clrArch}"
    "-DCLR_CMAKE_PGO_INSTRUMENT=0"
    "-DCLR_CMAKE_PGO_OPTIMIZE=0"
    "-DCMAKE_POLICY_VERSION_MINIMUM=3.5"
    # The lttng-ust library is linked only into libcoreclrtraceptprovider, which is not parsed.
    "-DLTTNG=/stub/liblttng-ust.so"
  ];

  graphName = runtimeName: targetName: "NativeCallGraph.${runtimeName}.${targetName}.tsv";

  graph = runtimeName: pin: targetName: let
    target = targets.${targetName};
    root = sysroot targetName target;
    build = "artifacts/obj/coreclr/linux.${target.clrArch}.Release";
  in
    pkgs.stdenvNoCC.mkDerivation {
      name = "native-call-graph-${runtimeName}-${targetName}";
      src = runtimeSource pin;
      patches = [./coreclr-configure.patch];
      # CoreCLR's configure looks for the LLVM tools it uses by name.
      nativeBuildInputs = [pkgs.cmake pkgs.ninja pkgs.python3 llvm.clang-unwrapped llvm.llvm llvm.lld];
      dontUpdateAutotoolsGnuConfigScripts = true;
      dontConfigure = true;
      dontFixup = true;
      buildPhase = ''
        runHook preBuild
        mkdir -p artifacts/obj
        cp ${versionStubs pin}/* artifacts/obj/
        export ROOTFS_DIR=${root}
        export TARGET_BUILD_ARCH=${target.clrArch}
        cmake -G Ninja -S src/coreclr -B ${build} \
          -C eng/native/tryrun.cmake \
          -DCMAKE_TOOLCHAIN_FILE=${toolchainFile targetName target} \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
          ${lib.escapeShellArgs (configureFlags target)} > configure.log 2>&1 \
          || { cat configure.log; exit 1; }
        python3 ${./program.py} ${build} units.txt libcoreclr.so libclrgc.so
        python3 ${./requests.py} ${contractTable} ${contractCallees} > requests.tsv
        mkdir -p $out
        python3 ${./generate.py} --tool ${tool}/bin/native-call-graph --db ${build}/compile_commands.json \
          --units units.txt --requests requests.tsv --root "$PWD" \
          --prefix sysroot=${root} --prefix clang=${resourceDir} \
          --jobs "''${NIX_BUILD_CORES:-1}" \
          --header "runtime: ${runtimeName}" \
          --header "commit: ${pin.sourceRev}" \
          --header "target: ${targetName}" \
          --header "program: the units of libcoreclr.so and libclrgc.so, which define no DACCESS_COMPILE" \
          --header "compiler: clang ${llvm.clang-unwrapped.version}, against glibc ${root.glibcVersion} and libstdc++ ${root.gccVersion}" \
          --header "configure: ${lib.concatStringsSep " " (configureFlags target)}" \
          --out $out/${graphName runtimeName targetName}
        runHook postBuild
      '';
      installPhase = "true";
    };

  # Each runtime's graph for each target, by runtime-target.
  cases = lib.concatMapAttrs (runtimeName: pin:
    lib.mapAttrs' (targetName: _:
      lib.nameValuePair "${runtimeName}-${targetName}" {
        file = graphName runtimeName targetName;
        built = graph runtimeName pin targetName;
      })
    targets)
  runtimes;

  graphChecks = lib.mapAttrs' (name: case:
    lib.nameValuePair "native-call-graph-${name}"
    (pkgs.runCommand "native-call-graph-${name}-matches" {} ''
      if ! diff -u ${graphDirectory + "/${case.file}"} ${case.built}/${case.file}; then
        echo "${case.file} is not the graph the generator writes. Regenerate it with" >&2
        echo "  nix build .#native-call-graph-${name} && cp result/${case.file} WoofWare.PawPrint.Test/NativeCallGraph/" >&2
        exit 1
      fi
      touch $out
    ''))
  cases;

  # Copies the oracle's program into ./oracle, enters it, and writes its compilation database.
  prepareOracle = ''
    cp -r ${./oracle} oracle
    chmod -R u+w oracle
    cd oracle
    root=$PWD
    cxx='"--target=x86_64-linux-gnu", "-std=c++17", "-nostdinc", "-nostdinc++", "-c"'
    cxx11='"--target=x86_64-linux-gnu", "-std=c++11", "-nostdinc", "-nostdinc++", "-c"'
    c='"--target=x86_64-linux-gnu", "-std=c11", "-nostdinc", "-c"'
    cat > compile_commands.json <<EOF
    [
      {"directory": "$root", "file": "$root/probes.cpp", "output": "probes.o", "arguments": ["clang++", $cxx, "probes.cpp", "-o", "probes.o"]},
      {"directory": "$root", "file": "$root/elsewhere.cpp", "output": "elsewhere.o", "arguments": ["clang++", $cxx, "elsewhere.cpp", "-o", "elsewhere.o"]},
      {"directory": "$root", "file": "$root/probes11.cpp", "output": "probes11.o", "arguments": ["clang++", $cxx11, "probes11.cpp", "-o", "probes11.o"]},
      {"directory": "$root", "file": "$root/cunit.c", "output": "cunit.o", "arguments": ["clang", $c, "cunit.c", "-o", "cunit.o"]}
    ]
    EOF
  '';

  oracle = pkgs.runCommand "native-call-graph-oracle" {nativeBuildInputs = [pkgs.python3];} ''
    ${prepareOracle}
    python3 ${./generate.py} --tool ${tool}/bin/native-call-graph --db compile_commands.json \
      --requests requests.tsv --root "$root" --header oracle --out graph.tsv
    grep -v '^#' graph.tsv | awk -F '\t' -v OFS='\t' '
      $1 == "routine" { print $1, $2, $3, $4, $6; next }
      $1 == "call" { print $1, $2, $3, $5, $6, $7, $8, $9, $11; next }
      { print }' > actual.tsv
    grep -v '^#' expected.tsv > wanted.tsv
    diff -u wanted.tsv actual.tsv
    touch $out
  '';
  # The tool with its walk wrong in each way the comparison with clang's IR must refuse by itself,
  # with the oracle's own expected graph not consulted: each a string of callgraph.cpp, what to
  # put in its place, and what the refusal must say.
  wrongWalks = {
    without-cleanup-deletes = {
      from = ''emit(E->getBeginLoc(), "delete", Cleanup, "-");'';
      to = "(void)Cleanup;";
      refusal = "AllocateCleanup.*calls what the walk does not name.*operator delete";
    };
    with-trivial-constructions = {
      from = "if (!Ctor->isTrivial())";
      to = "if (true)";
      refusal = "Trivial names what its IR does not call: Plain::Plain";
    };
    without-pointer-calls = {
      from = "W.Indirect.insert(E);";
      to = "(void)E;";
      refusal = "calls through a pointer, and the walk 0";
    };
    without-throws = {
      from = "(Thrown ? W.Throws : W.Rethrows) = true;";
      to = "(void)Thrown;";
      refusal = "does not find a .*throw, and its IR calls the runtime for it";
    };
  };

  crossCheck = pkgs.runCommand "native-call-graph-cross-check" {nativeBuildInputs = [pkgs.python3];} (''
      ${prepareOracle}
    ''
    + lib.concatStrings (lib.mapAttrsToList (name: wrong: let
        wrongTool = tool.overrideAttrs (old: {
          pname = "native-call-graph-${name}";
          postPatch = ''
            substituteInPlace callgraph.cpp --replace-fail ${lib.escapeShellArg wrong.from} ${lib.escapeShellArg wrong.to}
          '';
        });
      in ''
        if python3 ${./generate.py} --tool ${wrongTool}/bin/native-call-graph \
          --db compile_commands.json --requests requests.tsv --root "$root" --out graph.tsv 2> refusal.txt; then
          echo "The comparison with the IR accepted the walk ${name}." >&2
          exit 1
        fi
        if ! grep -q ${lib.escapeShellArg wrong.refusal} refusal.txt; then
          echo "The generator refused the walk ${name}, but not for what it gets wrong:" >&2
          cat refusal.txt >&2
          exit 1
        fi
        echo "${name}:"
        cat refusal.txt
      '')
      wrongWalks)
    + ''
      touch $out
    '');
in {
  inherit tool;
  graphs = lib.mapAttrs' (name: case: lib.nameValuePair "native-call-graph-${name}" case.built) cases;
  checks =
    graphChecks
    // {
      native-call-graph-oracle = oracle;
      native-call-graph-cross-check = crossCheck;
    };
}
