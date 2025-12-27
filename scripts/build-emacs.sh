#!/usr/bin/env bash
set -euo pipefail

# ---------------- defaults ----------------
EMACS_VERSION="emacs-31"
SRC_DIR="$HOME/src/emacs"
PREFIX="/usr/local"
JOBS="$(nproc)"
# ------------------------------------------

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  -v, --version <ver>     Emacs branch, tag, or commit (default: emacs-31)
  -s, --src <dir>         Emacs source directory (default: ~/src/emacs)
  -p, --prefix <dir>      Install prefix (default: /usr/local)
  -h, --help              Show this help

Note: This script builds Emacs with Wayland (pgtk) support only, no X11.
EOF
}

# ---------------- parse args ----------------
while [[ $# -gt 0 ]]; do
  case "$1" in
    -v|--version)
      EMACS_VERSION="$2"
      shift 2
      ;;
    -s|--src)
      SRC_DIR="$2"
      shift 2
      ;;
    -p|--prefix)
      PREFIX="$2"
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1"
      usage
      exit 1
      ;;
  esac
done
# -------------------------------------------

echo "==> Emacs build configuration"
echo "Version      : ${EMACS_VERSION}"
echo "Source dir   : ${SRC_DIR}"
echo "Install to   : ${PREFIX}"
echo "Backend      : Wayland (pgtk)"
echo

# ---------------- dependencies ----------------
echo "==> Installing dependencies"

sudo apt update
sudo apt install -y \
  build-essential autoconf texinfo git cmake \
  libgccjit-13-dev \
  libjansson-dev libtree-sitter-dev \
  libgtk-3-dev \
  libjpeg-dev libpng-dev libtiff-dev libgif-dev \
  libncurses-dev libxml2-dev libsqlite3-dev \
  librsvg2-dev \
  libwebkit2gtk-4.1-dev \
  libharfbuzz-dev libotf-dev \
  librime-dev \
  libtool-bin \ 
  imagemagick libmagickwand-dev
# ------------------------------------------------

# ---------------- source ----------------
mkdir -p "$(dirname "${SRC_DIR}")"

if [[ ! -d "${SRC_DIR}/.git" ]]; then
  echo "==> Cloning Emacs"
  git clone https://git.savannah.gnu.org/git/emacs.git "${SRC_DIR}"
fi

cd "${SRC_DIR}"
git fetch --tags
git checkout "${EMACS_VERSION}"
# ---------------------------------------

# ---------------- build ----------------
echo "==> Generating build system"
./autogen.sh

CONFIG_FLAGS=(
  "--prefix=${PREFIX}"
  "--with-native-compilation"
  "--with-tree-sitter"
  "--with-json"
  "--with-threads"
  "--with-modules"
  "--with-harfbuzz"
  "--with-cairo"
  "--with-rsvg"
  "--with-imagemagick"
  "--with-sqlite3"
  "--with-pgtk"
  "--without-x"
)

echo "==> Configuring Emacs"
./configure "${CONFIG_FLAGS[@]}"

echo "==> Building Emacs"
make -j"${JOBS}"

echo "==> Installing Emacs"
sudo make install
# ---------------------------------------

# ---------------- verify ----------------
echo
echo "==> Build complete"
"${PREFIX}/bin/emacs" --version
# ---------------------------------------
