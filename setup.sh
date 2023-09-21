set -eu

NIGHTLY_URL=https://moshell.dev/releases/nightly

BIN_PATH=~/.local/bin
LIBS_PATH=~/.local/share/moshell

NIGHTLY_VERSION=$(curl -s $NIGHTLY_URL/nightly-version)

# Use moshell's bin last modification date to determine if the binaries are up to date
if [ -e $BIN_PATH/moshell ] && [ "$(date -r $BIN_PATH/moshell +"%D")" = "$NIGHTLY_VERSION" ]; then
  echo Your Moshell version is up to date "($NIGHTLY_VERSION)"
  exit 0
fi

mkdir -p $BIN_PATH $LIBS_PATH

# setup binaries
echo Setting up Moshell nightly version of "$NIGHTLY_VERSION"
echo Downloading $NIGHTLY_URL/bin ...
wget -qc $NIGHTLY_URL/moshell -P $BIN_PATH
chmod +x $BIN_PATH/moshell
[ -e $BIN_PATH/msh ] || ln -s $BIN_PATH/moshell $BIN_PATH/msh

# setup standard library
echo Downloading $NIGHTLY_URL/lib.zip ...
wget -qc $NIGHTLY_URL/lib.zip -P $LIBS_PATH
echo Unzipping library
[ -e $LIBS_PATH/lib ] && rm -r "$LIBS_PATH/lib"
unzip -q $LIBS_PATH/lib.zip -d $LIBS_PATH && rm $LIBS_PATH/lib.zip


echo Moshell Nightly version "$NIGHTLY_VERSION" Successfully installed!
echo "Binary : $BIN_PATH/moshell"
echo "Stdlib : $LIBS_PATH/lib"
