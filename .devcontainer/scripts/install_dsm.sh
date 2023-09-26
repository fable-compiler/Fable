#!/bin/bash

# set -e

OS=$(echo `uname -s`|tr '[:upper:]' '[:lower:]')

case "$OS" in
    mingw*) OS='windows';;
    msys*) OS='windows';;
esac

VERSION="latest"
INSTALL_DIR="$HOME/.dsm"
FILE=""

# TODO: Implement this
show_help() {
  echo "dsm installation script"
  echo ""
  echo "USAGE:"
  echo "  ./install.sh [OPTIONS]"
  echo ""
  echo "OPTIONS:"
  echo "  -d, --install-dir Directory to install to."
  echo "  -s, --skip-shell Skip shell setup"
  echo "  -v, --version The version of dsm to install"
  echo "  -F, --filename Manually override the filename of dsm to use."
  echo ""
  echo "For any queries, help or bug, please open an issue at https://github.com/Yakiyo/dsm/issues"

  exit 0
}

clap() {
  while [[ $# -gt 0 ]]; do
    key="$1"

    case $key in
    -h | --help)
      show_help
      ;;
    -d | --install-dir)
      INSTALL_DIR="$2"
      shift
      shift
      ;;
    -s | --skip-shell)
      SKIP_SHELL="true"
      shift
      ;;
    -v | --version)
      VERSION="v$2"
      shift
      shift
      ;;
    -F | --filename)
      FILENAME="$2"
      shift
      shift
      ;;
    *)
      echo "Unrecognized argument $key. Use the `--help` flag to see usage"
      exit 1
      ;;
    esac
  done
}

# Duct taped this to somewhat work
filename() {
  if [ "$OS" = "linux" ]; then
    case "$(uname -m)" in
      arm | armv7*)
        FILENAME="arm-unknown-linux-gnueabihf"
        ;;
      *)
        FILENAME="x86_64-unknown-linux-musl"
    esac
  elif [ "$OS" = "darwin" ]; then
    FILENAME="x86_64-apple-darwin"
  elif [ "$OS" = "windows" ]; then
    FILENAME="x86_64-pc-windows-msvc.exe"
  else
    echo "OS $OS is not supported."
    echo "If you think that's a bug - please file an issue to https://github.com/Yakiyo/dsm/issues"
    exit 1
  fi
  FILE="dsm-$FILENAME"
}

download () {
  if [ "$VERSION" = "latest" ]; then
    URL="https://github.com/Yakiyo/dsm/releases/latest/download/$FILE"
  else
    URL="https://github.com/Yakiyo/dsm/releases/download/$VERSION/$FILE"
  fi
  echo "Downloading from $URL ...."

  TMP=$(mktemp -d)
  mkdir -p "$INSTALL_DIR" &>/dev/null

  if ! curl --progress-bar --fail -L "$URL" -o "$TMP/$FILE"; then
    echo "Download failed.  Check that the release/filename are correct."
    exit 1
  fi

  EXE=""
  if [ "$OS" = "windows" ]; then
    EXE=".exe"
  fi

  mv "$TMP/$FILE" "$INSTALL_DIR/dsm$EXE"

  chmod u+x "$INSTALL_DIR/dsm$EXE"
}

ensure_dir () {
  local CONTAINING_DIR
  CONTAINING_DIR="$(dirname "$1")"
  if [ ! -d "$CONTAINING_DIR" ]; then
    echo " >> Creating directory $CONTAINING_DIR"
    mkdir -p "$CONTAINING_DIR"
  fi
}

# Courtesy of https://github.com/Schniz/fnm/blob/master/.ci/install.sh
setup_shell() {
  CURRENT_SHELL="$(basename "$SHELL")"

  if [ "$CURRENT_SHELL" = "zsh" ]; then
    CONF_FILE=${ZDOTDIR:-$HOME}/.zshrc
    ensure_dir "$CONF_FILE"
    echo "Installing for Zsh. Appending the following to $CONF_FILE:"
    echo ""
    echo '  # dsm'
    echo '  export PATH="'"$INSTALL_DIR"':$PATH"'
    echo '  eval "`dsm env zsh`"'

    echo '' >>$CONF_FILE
    echo '# dsm' >>$CONF_FILE
    echo 'export PATH="'$INSTALL_DIR':$PATH"' >>$CONF_FILE
    echo 'eval "`dsm env zsh`"' >>$CONF_FILE

  elif [ "$CURRENT_SHELL" = "fish" ]; then
    CONF_FILE=$HOME/.config/fish/conf.d/dsm.fish
    ensure_dir "$CONF_FILE"
    echo "Installing for Fish. Appending the following to $CONF_FILE:"
    echo ""
    echo '  # dsm'
    echo '  set PATH "'"$INSTALL_DIR"'" $PATH'
    echo '  dsm env fish | source'

    echo '# dsm' >>$CONF_FILE
    echo 'set PATH "'"$INSTALL_DIR"'" $PATH' >>$CONF_FILE
    echo 'dsm env fish | source' >>$CONF_FILE

  elif [ "$CURRENT_SHELL" = "bash" ]; then
    if [ "$OS" = "darwin" ]; then
      CONF_FILE=$HOME/.profile
    else
      CONF_FILE=$HOME/.bashrc
    fi
    ensure_dir "$CONF_FILE"
    echo "Installing for Bash. Appending the following to $CONF_FILE:"
    echo ""
    echo '  # dsm'
    echo '  export PATH="'"$INSTALL_DIR"':$PATH"'
    echo '  eval "`dsm env bash`"'

    echo '' >>$CONF_FILE
    echo '# dsm' >>$CONF_FILE
    echo 'export PATH="'"$INSTALL_DIR"':$PATH"' >>$CONF_FILE
    echo 'eval "`dsm env bash`"' >>$CONF_FILE

  else
    echo "Could not infer shell type. Please set up manually."
    exit 1
  fi

  echo ""
  echo "In order to apply the changes, open a new terminal or run the following command:"
  echo ""
  echo "  source $CONF_FILE"
}

filename
clap "$@"
download
if [ "$SKIP_SHELL" != "true" ]; then
  setup_shell
fi