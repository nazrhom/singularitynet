#!/usr/bin/env bash

# Exit if any command throws an error
set -e

# Get the project and module names from command line args
# Probably want to make it so that we only need to provide one later
case $# in
  0)
    read -p "Enter the project name (e.g. gero-gov): " PROJECT_NAME
    read -p "Enter the module name (e.g. GeroGov): " MODULE_NAME
    NEW_GITHUB_LINK="https://github.com/mlabs-haskell/${PROJECT_NAME}"
    read -p "Enter the name of the github link (default: ${NEW_GITHUB_LINK}): " github_input
    if [[ $github_input != "" ]]
    then
      NEW_GITHUB_LINK=$github_input
    fi
  ;;

  2)
    PROJECT_NAME=$1
    MODULE_NAME=$2
    NEW_GITHUB_LINK="https://github.com/mlabs-haskell/${PROJECT_NAME}"
    read -p "Enter the name of the github link (default: ${NEW_GITHUB_LINK}): " github_input
    if [[ $github_input != "" ]]
    then
      NEW_GITHUB_LINK=$github_input
    fi
  ;;

  3)
    PROJECT_NAME=$1
    MODULE_NAME=$2
    NEW_GITHUB_LINK=$3
  ;;

  *)
    echo "You may run this script with 0, 2, or 3 arguments"
    echo "E.g.:"
    echo "    ${0}"
    echo "    ${0} gero-gov GeroGov"
    echo "    ${0} gero-gov GeroGov https://github.com/mlabs-haskell/gero-gov"
    exit 1
  ;;
esac

# Sed runs differently on Mac and Linux. Determine which one we're on
SED_COMMAND="sed -i "
if [[ $OSTYPE == 'darwin'* ]]
  then
    SED_COMMAND="sed -i'' "
fi

# Modify the cabal files
OLD_GITHUB_LINK="https://github.com/template-project"
for file in $(find -name "template-project*.cabal"); do
  new_cabal_file=$(echo $file | sed "s|template-project|${PROJECT_NAME}|g")
  mv $file $new_cabal_file
  git add $file
    
  $SED_COMMAND "s|${OLD_GITHUB_LINK}|${NEW_GITHUB_LINK}|g" $new_cabal_file
  $SED_COMMAND "s|template-project|${PROJECT_NAME}|g" $new_cabal_file
  $SED_COMMAND "s|TemplateProject|${MODULE_NAME}|g" $new_cabal_file
  git add $new_cabal_file
done

# Modify flake.nix, Makefile, and integrate.yaml
$SED_COMMAND "s|template-project|${PROJECT_NAME}|g" flake.nix
$SED_COMMAND "s|template-project|${PROJECT_NAME}|g" Makefile
$SED_COMMAND "s|template-project|${PROJECT_NAME}|g" .github/workflows/integrate.yaml
git add flake.nix Makefile .github/workflows/integrate.yaml

# Modify the dummy source files and Spec files
for file in $(grep -rl TemplateProject ./*/); do
  $SED_COMMAND "s|TemplateProject|${MODULE_NAME}|g" $file
  git add $file
done
for file in $(find -name "*TemplateProject*"); do
  new_file=$(echo $file | sed "s|TemplateProject|${MODULE_NAME}|g")
  mv $file $new_file
  git add $file $new_file
done

# Create the cabal.project.local file with the sodium flag
echo "package cardano-crypto-praos" >> cabal.project.local
echo "  flags: -external-libsodium-vrf" >> cabal.project.local

# Commit to make sure nix is aware of renamed files
git commit -m "Ran setup.sh"

# Make sure the permissions on format.sh are correct
chmod 755 .github/format.sh

# Perform first build and test
nix develop .#onchain --command bash -c "cd onchain && cabal build && cabal test"
nix develop .#offchain --command bash -c "cd offchain && cabal build && cabal test"

# Perform CI actions
nix develop .#onchain --command make lint
nix develop .#onchain --command make format
nix develop .#offchain --command make format
nix build .#check.x86_64-linux

echo "Successfully renamed and built project. A commit containing the changes has already been added (but not pushed)."
echo "Happy coding!"
