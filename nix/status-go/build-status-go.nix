{ buildGoPackage, go, xcodeWrapper, stdenv, utils }:

{ owner, repo, rev, cleanVersion, goPackagePath, src, host,
  nativeBuildInputs ? [],
  buildInputs ? [],
  buildPhase, buildMessage,
  installPhase ? "",
  postInstall ? "",
  preFixup ? "",
  outputs, meta } @ args':

with stdenv;

let
  inherit (stdenv.lib) strings;

  removeReferences = [ go ];
  removeExpr = refs: ''remove-references-to ${lib.concatMapStrings (ref: " -t ${ref}") refs}'';

  args = removeAttrs args' [ "buildMessage" ]; # Remove our arguments from args before passing them on to buildGoPackage
  buildStatusGo = buildGoPackage (args // {
    pname = repo;
    version = "${cleanVersion}-${strings.substring 0 7 rev}-${host}";

    nativeBuildInputs =
      nativeBuildInputs ++
      lib.optional isDarwin xcodeWrapper;
    inherit buildInputs;

    # Fixes Cgo related build failures (see https://github.com/NixOS/nixpkgs/issues/25959 )
    hardeningDisable = [ "fortify" ];

    # Ensure XCode is present, instead of failing at the end of the build
    preConfigure = lib.optionalString isDarwin utils.enforceXCodeAvailable;

    buildPhase = ''
      runHook preBuild

      runHook renameImports

      echo
      echo "${buildMessage}"
      echo
      ${buildPhase}

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      ${installPhase}

      runHook postInstall
    '';

    # replace hardcoded paths to go package in /nix/store, otherwise Nix will fail the build
    preFixup = preFixup + ''
      find $out -type f -exec ${removeExpr removeReferences} '{}' + || true
      return # make sure we stop fixup so as to not allow the host preFixup script to proceed
    '';

    passthru = { inherit owner repo version rev; };

    meta = {
      # Add default meta information
      inherit (meta) platforms;
      description = meta.description or "The Status module that consumes go-ethereum.";
      license = lib.licenses.mpl20;
    } // meta // {
      # add an extra maintainer to every package
      maintainers = (meta.maintainers or [ ]) ++ [ lib.maintainers.pombeirp ];
    };
  });

in buildStatusGo
