{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "iele-assemble"; version = "0.0.1.0"; };
      license = "LicenseRef-NCSA";
      copyright = "2017 Runtime Verification";
      maintainer = "virgil.serbanuta@runtimeverification.com";
      author = "Virgil Serbanuta";
      homepage = "https://github.com/runtimeverification/iele-semantics#readme";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."validation" or (errorHandler.buildDepError "validation"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."sandi" or (errorHandler.buildDepError "sandi"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ];
        buildable = true;
        modules = [
          "IeleTHUtil"
          "IeleParser"
          "IeleDesugar"
          "IelePrint"
          "IeleTypes"
          "IeleInstructions"
          "IeleAssembler"
          "IeleParserImplementation"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "iele-assemble" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."iele-assemble" or (errorHandler.buildDepError "iele-assemble"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."sandi" or (errorHandler.buildDepError "sandi"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "iele-assemble-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."iele-assemble" or (errorHandler.buildDepError "iele-assemble"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          hsSourceDirs = [ "test" ];
          mainPath = [ "TestParser.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }