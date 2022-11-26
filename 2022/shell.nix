{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    julia-bin
    racket
  ];
  shellHook = ''
  '';
}
