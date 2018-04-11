{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  pbenchSrc ? ../.,
  ocaml ? pkgs.ocaml,
  gcc ? pkgs.gcc,
  R ? pkgs.R,
  texlive ? pkgs.texlive.combined.scheme-small,
  pandoc ? pkgs.pandoc,
  makeWrapper ? pkgs.makeWrapper,
  buildDocs ? false
}:

# Later: consider factoring pbench the command-line tool
# and pbench the library separately.

stdenv.mkDerivation rec {
  name = "pbench";

  src = pbenchSrc;

  buildInputs =
    let docs =
      if buildDocs then [ pandoc texlive] else [];
    in
    [ ocaml gcc R texlive makeWrapper ] ++ docs;

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
    wrapProgram $out/bin/pplot \
      --prefix PATH ":" ${texlive}/bin \
      --prefix PATH ":" ${R}/bin
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
