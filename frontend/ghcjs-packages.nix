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

    # webkit = self.callPackage ({ mkDerivation }:
    #   mkDerivation {
    #     pname = "webkitgtk3";
    #     version = "0.14.1.2";
    #     src = nixpkgs.fetchgit {
    #       url = "https://github.com/gtk2hs/webkit";
    #       rev = "03f58e1e9a53d98a587023b3359bfd8da3b3c7ab";
    #       sha256 = "0qb8cra8q5lhp6pd3dll0i4waza36kk5hig6jf36iyiqsls93bcj";
    #     };

    #     buildDepends = [
    #     ];

    #     license = null;
    #   }

    # ) {};


    #    file-embed = self.callPackage ({ mkDerivation }:
    #      mkDerivation {
    #        pname = "file-embed";
    #        version = "0.0.10";
    #        src = nixpkgs.fetchgit {
    #         url = "https://github.com/snoyberg/file-embed";
    #         rev = "abcf9018bd8b10f7147e8a7383faf2872b0e3051";
    #         sha256 = "139npi667vfn06q01bmj3x37pxv00xhwjs7dxvkc8y3dmblffk4s";
    #        };
    #
    #        buildDepends = [ ];
    #
    #        license = null;
    #      }
    #
    #    ) {};

  };
}
