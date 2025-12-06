{ pkgs, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.packages.ghc9103.ghcWithPackages (ps: with ps; [
      aeson
      evdev
      free
      hspec
      network-bsd
      polysemy
      QuickCheck
    ]);
  };

  packages = with pkgs; [
    ghcid
    libevdev
  ];

  env.GHC_OPTIONS = pkgs.lib.join " " [
    "-Wall"
    "-O2"
    "-flate-specialise"
    "-fspecialise-aggressively"
    "-fplugin=Polysemy.Plugin"
    "-XDataKinds"
    "-XFlexibleContexts"
    "-XGADTs"
    "-XLambdaCase"
    "-XPolyKinds"
    "-XRankNTypes"
    "-XScopedTypeVariables"
    "-XTypeApplications"
    "-XTypeOperators"
    "-XTypeFamilies"
  ];

  scripts = {
    build.exec = "mkdir -p output && ghc -Wall -outputdir output -o output/main -i=src src/AltMain.hs";
    run.exec = "ghc -i=src --run src/AltMain.hs -- \"$@\"";
    tests.exec = "ghc -i=src -i=test --run test/Test/Main.hs";
    watch.exec = "ghcid --test=Test.Main.main --lint=lint";
    format.exec = "ormolu --mode inplace $(find {src,test} -name '*.hs')";
    lint.exec = "hlint {src,test}";
    docs.exec = "haddock --html --no-warnings -o docs $(find src -name '*.hs')";
  };

  git-hooks.hooks = {
    ormolu.enable = true;
    hlint.enable = true;
  };
}
