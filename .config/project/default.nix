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
    ## TODO: Move something like this to Flaky.
    file = let
      copyLicenses = dir: {
        "${dir}/LICENSE".source = ../../LICENSE;
        "${dir}/LICENSE.AGPL-3.0-only".source = ../../LICENSE.AGPL-3.0-only;
        "${dir}/LICENSE.Universal-FOSS-exception-1.0".source =
          ../../LICENSE.Universal-FOSS-exception-1.0;
        "${dir}/LICENSE.commercial".source = ../../LICENSE.commercial;
      };
    in
      copyLicenses "calendrical"
      // copyLicenses "mixed-radix"
      // copyLicenses "numeric-tangle"
      // copyLicenses "numeric-tangle-fin";
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
    extraDependencyVersions = ["doctest-0.24.0"];
    latestGhcVersion = "9.10.1";
  };

  ## publishing
  services.github.settings.repository.topics = ["calendar" "date"];
}
