{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  fetchurl,
  buildDocs ? false
}:

stdenv.mkDerivation rec {
  name = "pbench-${version}";
  version = "v1.0";

  src = fetchurl {
    url = "https://github.com/deepsea-inria/pbench/archive/${version}.tar.gz";
    sha256 = "0qwbpaygrhh3n45rs5f5ky5d526y4jjp54j5p7vgs5n8nz5ircz0";
  };

  buildInputs =
    let docs =
      if buildDocs then [
        pkgs.pandoc
        pkgs.texlive.combined.scheme-full
      ] else
        [];
    in
    [ pkgs.ocaml ] ++ docs;

  buildPhase =
    let docs = 
      if buildDocs then ''
        make pplot.pdf prun.pdf
      ''
      else ''
        # nothing to build
      '';
    in
    ''
    make
    ${docs}
    '';

  installPhase =
    let docs =
      if buildDocs then ''
        cp *.pdf $out/docs
      ''
      else ''
        # nothing to copy
      '';
    in
    ''
    mkdir -p $out 
    cp *.ml Makefile Makefile_common timeout.c $out/
    mkdir -p $out/bin/
    cp prun pplot prun.pbench pplot.pbench timeout.out $out/bin/
    mkdir -p $out/lib/
    cp lib/*.ml $out/lib/
    mkdir -p $out/tools/
    cp tools/*.tex $out/tools/
    mkdir -p $out/xlib/
    cp xlib/*.ml $out/xlib/
    mkdir -p $out/docs/
    cp *.md $out/docs/
    ${docs}
    '';

  meta = {
    description = "Parallel algorithm benchmarking toolkit.";
    license = "MIT";
    homepage = http://deepsea.inria.fr/pbench/;
  };
}