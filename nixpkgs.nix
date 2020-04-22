/*
NIXPKGS version

Any archive of nixpkgs can be used.

The simplest update solution is to look at
http://github.com/NixOS/nixpkgs-channels and pick the latest commit for
nixpkgs-unstable. The archive can then be fetched at:

https://github.com/NixOS/nixpkgs-channels/archive/COMMIT_NUMBER.tar.gz;

and the control sum computed using `sha256`.
*/

let
  # nixpkgs-unstable 2020-01-30
  sha256 = "089hqg2r2ar5piw9q5z3iv0qbmfjc4rl5wkx9z16aqnlras72zsa";
  rev = "22a3bf9fb9edad917fb6cd1066d58b5e426ee975";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
