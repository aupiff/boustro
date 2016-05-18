{ reflex-platform, ... }:
let inherit (reflex-platform) nixpkgs;
in reflex-platform.ghcjs.override {
  overrides = self: super: {
    ghcjs-jquery = self.callPackage ({ mkDerivation, data-default, ghcjs-base, ghcjs-dom, text }:
      mkDerivation {
        pname = "ghcjs-jquery";
        version = "0.1.0.0";
        src = nixpkgs.fetchgit {
         url = "https://github.com/ghcjs/ghcjs-jquery";
         rev = "6e8023229342eaf78204cde3189865be891e5aa4";
         sha256 = "0i03p1mv82r01wiz4l4ysb4znavriva2zyxc713ds6y35nf8xpzm";
        };
        buildDepends = [
          data-default ghcjs-base ghcjs-dom text
        ];
        jailbreak = true;
        license = null;
      }
    ) {};

    hyphenation = self.callPackage ({ mkDerivation, unordered-containers }:
      mkDerivation {
        pname = "hyphenation";
        version = "0.6.1";
        src = nixpkgs.fetchgit {
         url = "https://github.com/aupiff/hyphenation";
         rev = "6371bdc3446e7eb6fe74e24922e812f41af2a8e";
         sha256 = "1crr75kg1mn69x4c0wa91d81n8shp3h01nk99fxjx0zvnhpcla9k";
        };

        buildDepends = [
          unordered-containers
        ];

        license = null;
      }

    ) {};
  };
}
