{
  description = "Acapella dev shell";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

  outputs = { self, nixpkgs, ... }:
  let
    forAllSystems = f: nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" ] (system:
      f (import nixpkgs { inherit system; }));
  in {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          emacs30-nox   # or emacs-gtk if you prefer GUI
          cask
          git
          cacert        # TLS roots for MELPA/ELPA
          curl          # your SSE transport uses curl
        ];
        shellHook = ''
          export CASK_EMACS=$(command -v emacs)
          export EMACS=$(command -v emacs)
        '';
      };
    });
  };
}
