{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, stdenv, template-haskell, transformers
      }:
      mkDerivation {
        pname = "haskell-holes-th";
        version = "1.0.0.0";
        sha256 = "13xyxck9f15mwi641zs9zw77cnrgh30p2771f66haby96k8wx9jf";
        libraryHaskellDepends = [ base template-haskell transformers ];
        homepage = "https://github.com/8084/haskell-holes-th";
        description = "Infer haskell code by given type";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
