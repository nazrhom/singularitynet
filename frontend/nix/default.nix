{ src
, pkgs
, system
, inputs
}:

let
  ps-lib = import ./lib.nix {
    inherit pkgs easy-ps spagoPkgs nodejs nodeModules;
  };
  # We should try to use a consistent version of node across all
  # project components
  nodejs = pkgs.nodejs-12_x;
  easy-ps = import inputs.easy-purescript-nix { inherit pkgs; };
  spagoPkgs = import ../spago-packages.nix { inherit pkgs; };
  nodeEnv = import
    (pkgs.runCommand "nodePackages"
      {
        buildInputs = [ pkgs.nodePackages.node2nix ];
      } ''
      mkdir $out
      cp ${src}/package.json $out/package.json
      cp ${src}/package-lock.json $out/package-lock.json
      cd $out
      node2nix --lock package-lock.json
    '')
    { inherit pkgs nodejs system; };
  nodeModules =
    let
      modules = pkgs.callPackage
        (_:
          nodeEnv // {
            shell = nodeEnv.shell.override {
              # see https://github.com/svanderburg/node2nix/issues/198
              buildInputs = [ pkgs.nodePackages.node-gyp-build ];
            };
          });
    in
    (modules { }).shell.nodeDependencies;
  flake = {
    packages = {
      singularitynet-frontend-nodejs = ps-lib.buildPursProject {
        name = "singularitynet-frontend";
        subdir = "exe";
        inherit src;
      };
    };

    checks = {
      singularitynet-frontend = ps-lib.runPursTest {
        name = "singularitynet-frontend";
        subdir = "test";
        inherit src;
      };
    };

    devShell =
      pkgs.mkShell
        {
          buildInputs = with easy-ps; [
            easy-ps.purs-0_14_5
            spago
            purs-tidy
            purescript-language-server
            pscid
            spago2nix
            pkgs.nodePackages.node2nix
            nodejs
          ];

          shellHook = ''
            __ln-node-modules () {
              local modules=./frontend/node_modules
              if test -L "$modules"; then
                rm "$modules";
              elif test -e "$modules"; then
                echo 'refusing to overwrite existing (non-symlinked) `node_modules`'
                exit 1
              fi

              ln -s ${nodeModules}/lib/node_modules "$modules"
            }

            __ln-node-modules

            export PATH="${nodeModules}/bin:$PATH"
          '';
        };
  };
in
{ inherit flake; }
