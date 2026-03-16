#/bin/sh

# Usage:
# ./build.sh <VERSION:0.4.226> <ARCH:x64,arm64,arm32,x86> <ICON_PATH:./icon.png>

USAGE="./build.sh <VERSION:0.4.226> <ARCH:x64> <ICON_PATH:./icon.png>"

set -ex
VERSION=$1
ARCH=$2
ICON_PATH=$3
BUILDDIR=$PWD/builddir
URL="https://github.com/tranquilit/openrsat/releases/download/v"$VERSION

if [ "$#" -ne 3 ]; then
  echo "Invalid number of arguments."
  echo "Usage:"
  echo "	$USAGE"
  exit
fi

echo "Verify architecture..."

if [ "$ARCH" = "x64" ]; then
  RPMARCH=x86_64
elif [ "$ARCH" = "arm64" ]; then
  RPMARCH=arm64
elif [ "$ARCH" = "arm32" ]; then
  RPMARCH=arm32
elif [ "$ARCH" = "i386" ]; then
  RPMARCH=i386
else
  echo "Invalid architecture. Exit."
  exit
fi

echo "Setup build folder"
mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS}

echo "Setup download URL"
BINURL=$URL/OpenRSAT-linux-$ARCH

echo "Download from $URL to ~/rpmbuild/SOURCES/OpenRSAT"
wget -O ~/rpmbuild/SOURCES/OpenRSAT $BINURL

echo "Copy icon file"
[ -f $ICON_PATH ] || (echo "$ICON_PATH does not exists." && exit)
cp $ICON_PATH ~/rpmbuild/SOURCES/OpenRSAT.png

echo "Copy OpenRSAT.desktop file"
[ -f "./OpenRSAT.desktop" ] || (echo "./OpenRSAT.desktop does not exists." && exit)
cp ./OpenRSAT.desktop ~/rpmbuild/SOURCES/OpenRSAT.desktop

echo "Copy OpenRSAT.spec file"
[ -f "./OpenRSAT.spec" ] || (echo "./OpenRSAT.spec does not exists." && exit)
cp ./OpenRSAT.spec ~/rpmbuild/SPECS/OpenRSAT.spec

echo "Build rpm"
rpmbuild -bb --buildroot $BUILDDIR --define "_version $VERSION" --define "_rpmarch $RPMARCH" --clean ~/rpmbuild/SPECS/OpenRSAT.spec

echo "Copy built rpm file"
mv ~/rpmbuild/RPMS/*/*.rpm .

echo "Clear build workspace"
rm -rf ~/rpmbuild $BUILDDIR

echo "Done."