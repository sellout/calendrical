### All available options for this file are listed in
### https://sellout.github.io/project-manager/options.xhtml
{
  config,
  lib,
  self,
  ...
}: {
  project = {
    name = "calendrical";
    summary = "Comprehensive date & time library";
    file = let
      ## Cabal requires many files to exist at the package level, rather than
      ## the repo level. This makes copies of the individual files into the
      ## package directory.
      ##
      ## TODO: Move something like this to Flaky.
      perPackageFiles = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.proprietary".source = ../../LICENSE.proprietary;
        ## We might want to put this somewhere else (like .config/henforcer/),
        ## but that isn’t currently an option, because of flipstone/henforcer#7.
        "${dir}/henforcer.toml".text =
          lib.pm.generators.toTOML {} {
            globalSection = {};
            sections = {
              forAnyModule = {
                ## doesn’t yet support nested attr sets
                # allowedAliasUniqueness.allAliasesUniqueExcept = [];
                maximumExportsPlusHeaderUndocumented = 0;
                maximumExportsWithoutSince = 0;
                moduleHeaderCopyrightMustExistNonEmpty = true;
                ## We want to require a description, but just a “normal”
                ## description, not the header field.
                moduleHeaderDescriptionMustExistNonEmpty = false;
                moduleHeaderLicenseMustExistNonEmpty = true;
              };
            };
          }
          ## NB: `toTOML` is really just an INI generator, so it can’t handle a
          ##     lot of syntax. This tacks some bits onto the end that the INI
          ##     generator does’t like.
          + ''
            # Exclude auto-generated `Paths` module
            [[forPatternModules]]
            pattern = "Paths_*"
            [forPatternModules.rulesToIgnore]
            all = true

            # Exclude auto-generated `Build_doctests` module
            [[forSpecifiedModules]]
            module = "Build_doctests"
            [forSpecifiedModules.rulesToIgnore]
            all = true
          '';
      };
    in
      perPackageFiles "calendrical"
      // perPackageFiles "mixed-radix"
      // perPackageFiles "numeric-tangle"
      // perPackageFiles "numeric-tangle-fin";
  };

  imports = [./hlint.nix];

  ## CI
  ## FIXME: Shouldn’t need `mkForce` here (or to duplicate the base contexts).
  ##        Need to improve module merging.
  services.github.settings.branches.main.protection.required_status_checks.contexts =
    lib.mkForce
    ([
        "All Garnix checks"
        "check-bounds"
        "check-licenses"
      ]
      ++ lib.concatMap (sys:
        lib.concatMap (ghc: [
          "build (${ghc}, ${sys})"
          "build (--prefer-oldest, ${ghc}, ${sys})"
        ])
        self.lib.nonNixTestedGhcVersions)
      config.services.haskell-ci.systems);
  services.haskell-ci = {
    inherit (self.lib) defaultGhcVersion;
    ghcVersions = self.lib.nonNixTestedGhcVersions;
    cabalPackages = {
      mixed-radix = "mixed-radix";
      numeric-tangle = "numeric-tangle";
      numeric-tangle-fin = "numeric-tangle-fin";
      calendrical = "calendrical";
    };
    ## Used by Nix builds, but not by GitHub.
    checkBounds.extraDependencyVersions = [
      "QuickCheck-2.15.0"
      "doctest-0.24.0"
    ];
    ## The latest Stackage LTS that we also build on GitHub for.
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = ["calendar" "date"];
}
