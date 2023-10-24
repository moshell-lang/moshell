#!/bin/sh
# shellcheck shell=dash

set -eu

case "$(uname -s)" in
  Linux)
    OS=linux-gnu
    LIBS_PATH=~/.local/share/moshell
    ;;
  Darwin)
    OS=apple-darwin
    LIBS_PATH=~/Library/Application\ Support/moshell
    ;;
  *)
    echo "Unsupported $(uname -s) OS" >&2
    exit 1
    ;;
esac

case "$(uname -m)" in
  x86_64)
    TARGET="x86_64-$OS"
    ;;
  arm64)
    if [ "$OS" = "apple-darwin" ]; then
      TARGET=aarch64-apple-darwin
    else
      echo "Unsupported $(uname -m) architecture" >&2
      exit 1
    fi
    ;;
  *)
    echo "Unsupported $(uname -m) architecture" >&2
    exit 1
    ;;
esac

NIGHTLY_URL=https://moshell.dev/releases/nightly/$TARGET
BIN_PATH=~/.local/bin

NIGHTLY_VERSION=$(wget -qO- $NIGHTLY_URL/nightly-version)

# Use moshell's bin last modification date to determine if the binaries are up to date
if [ -e $BIN_PATH/moshell ] && [ "$(date -r $BIN_PATH/moshell +"%D")" = "$NIGHTLY_VERSION" ]; then
  echo Your Moshell version is up to date "($NIGHTLY_VERSION)"
  exit 0
fi

mkdir -p "$BIN_PATH" "$LIBS_PATH"

# setup binaries
echo Setting up Moshell nightly version of "$NIGHTLY_VERSION"
echo Downloading $NIGHTLY_URL/bin ...
wget -qc $NIGHTLY_URL/moshell -P "$BIN_PATH"
chmod +x "$BIN_PATH/moshell"
[ -e "$BIN_PATH/msh" ] || ln -s "$BIN_PATH/moshell" "$BIN_PATH/msh"

# setup standard library
echo Downloading $NIGHTLY_URL/lib.zip ...
wget -qc $NIGHTLY_URL/lib.zip -P "$LIBS_PATH"
echo Unzipping library
rm -rf "$LIBS_PATH/lib"
unzip -q "$LIBS_PATH/lib.zip" -d "$LIBS_PATH" && rm "$LIBS_PATH/lib.zip"


echo Moshell Nightly version "$NIGHTLY_VERSION" Successfully installed!
echo "Binary : $BIN_PATH/moshell"
echo "Stdlib : $LIBS_PATH/lib"

if ! echo "$PATH" | grep -q "$BIN_PATH"; then
  echo
  echo $BIN_PATH not found in '$PATH' variable
  echo Do you want to add $BIN_PATH in your '$PATH' ? [Y/n]
  read CHOICE

  case $(basename -- "$SHELL") in
      "bash")
      SHELLRC=~/.bashrc
    ;;
      "zsh")
      SHELLRC=~/.zshrc
    ;;
      "nu")
      SHELLRC=~/.config/nushell/env.nu
    ;;
      *)
        exit 0
  esac

  if echo "$CHOICE" | grep -qE "^\s*$" || [ "$CHOICE" = "Y" ] || [ "$CHOICE" = "y" ]; then
      echo export PATH="$PATH:$BIN_PATH" >> $SHELLRC
      echo your '$PATH' variable has been updated.
      echo you can use \'source $SHELLRC\' to apply the changes
  fi
fi
