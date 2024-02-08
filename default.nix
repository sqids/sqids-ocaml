let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "d537b64510125963be26c551b53723831cabb352";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;
  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };
  deps = { "ocaml-base-compiler" = "5.1.0"; };
}
