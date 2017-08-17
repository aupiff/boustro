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
         sha256 = "0idqg67zxr6yj1957nm55x07klllxfhahjbgp7zg6rdwha1vfvy6";
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
         sha256 = "00ckxiz0adr8xq3ix6alnwrlz786jp4p9yd6pybi4y8z1kbqbhbs";
        };

        buildDepends = [
          unordered-containers
        ];

        license = null;
      }

    ) {};

  };
}
