#!/usr/bin/env bash
set -euo pipefail

# ---------------- defaults ----------------
EMACS_VERSION="emacs-31"
SRC_DIR="$HOME/src/emacs"
PREFIX="/usr/local"
ENABLE_PGTK=0
JOBS="$(nproc)"
# ------------------------------------------

usage() {
  cat <<EOF
Usage: $0 [options]

Options:
  -v, --version <ver>     Emacs branch, tag, or commit (default: emacs-31)
  -s, --src <dir>         Emacs source directory (default: ~/src/emacs)
  -p, --prefix <dir>      Install prefix (default: /usr/local)
      --pgtk              Enable Wayland (pgtk) backend
  -h, --help              Show this help
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
    --pgtk)
      ENABLE_PGTK=1
      shift
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
echo "pgtk enabled : ${ENABLE_PGTK}"
echo

# ---------------- dependencies ----------------
echo "==> Installing dependencies"

sudo apt update
sudo apt install -y \
  build-essential autoconf texinfo git cmake \
  libgccjit-13-dev \
  libjansson-dev libtree-sitter-dev \
  libgtk-3-dev \
  libxpm-dev libjpeg-dev libpng-dev libtiff-dev libgif-dev \
  libncurses-dev libxml2-dev libsqlite3-dev \
  librsvg2-dev \
  libwebkit2gtk-4.1-dev \
  libharfbuzz-dev libotf-dev \
  librime-dev \
  libtool-bin \ 
  imagemagick libmagickwand-dev

if [[ ${ENABLE_PGTK} -eq 1 ]]; then
  sudo apt install -y libwebkit2gtk-4.1-dev
fi
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
  "--with-x-toolkit=gtk3"
)

if [[ ${ENABLE_PGTK} -eq 1 ]]; then
  CONFIG_FLAGS+=("--with-pgtk")
else
  CONFIG_FLAGS+=("--with-xwidgets")
fi

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
