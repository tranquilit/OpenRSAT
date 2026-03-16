#/bin/sh

# Usage:
# ./build.sh <VERSION:0.4.226> <ARCH:x64,arm64,arm32,x86> <ICON_PATH:./icon.png>

set -ex
VERSION=$1
ARCH=$2
ICON_PATH=$3
BUILDDIR=$PWD/builddir
URL="https://github.com/tranquilit/openrsat/releases/download/v"$VERSION

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

echo "Setting up environment"
mkdir -p ~/rpmbuild/{BUILD,RPMS,SOURCES,SPECS}

echo "Download from $URL"
wget -O ~/rpmbuild/SOURCES/OpenRSAT $URL/OpenRSAT-linux-$ARCH
cp $ICON_PATH ~/rpmbuild/SOURCES/OpenRSAT.png
cp ./OpenRSAT.desktop ~/rpmbuild/SOURCES/OpenRSAT.desktop
cp ./OpenRSAT.spec ~/rpmbuild/SPECS/OpenRSAT.spec

echo "Build rpm"
rpmbuild -bb --buildroot $BUILDDIR --define "_version $VERSION" --define "_rpmarch $RPMARCH" --clean ~/rpmbuild/SPECS/OpenRSAT.spec

echo "Retrieve rpm"
mv ~/rpmbuild/RPMS/*/*.rpm .

echo "Remove builddir"
rm -rf ~/rpmbuild $BUILDDIR

echo "Success"