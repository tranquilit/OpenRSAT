#!/bin/bash

# Usage:
# ./build.sh <VERSION:0.4.226> <ARCH:x64,arm64,arm32,x86> <ICON_PATH:./icon.png>

VERSION=$1
ARCH=$2
ICON_PATH=$3
URL="https://github.com/tranquilit/openrsat/releases/download/v"$1
BUILDDIR=$PWD/builddir

echo "Verify architecture..."

if [ "$ARCH" = "x64" ]; then
  DEBARCH=amd64
elif [ "$ARCH" = "arm64" ]; then
  DEBARCH=arm64
elif [ "$ARCH" = "arm32" ]; then
  DEBARCH=armhf
elif [ "$ARCH" = "x86" ]; then
  DEBARCH=i386
else
  echo "Invalid architecture. Exit."
  exit
fi

echo "Architecture OK. "$ARCH" -> "$DEBARCH

echo "Remove remaining .deb files."
#rm -rf *.deb $BUILDDIR

echo "Create folder structure."
mkdir -p $BUILDDIR/DEBIAN
mkdir -p $BUILDDIR/{opt/openrsat/,usr/share/applications}

echo "Copy control and postinst files."
cp ./control $BUILDDIR/DEBIAN
cp ./postinst $BUILDDIR/DEBIAN

echo "Set permissions to postinst and control."
chmod 755 $BUILDDIR/DEBIAN/postinst
chmod 755 $BUILDDIR/DEBIAN/control

echo "Change version in control file."
sed "s/VERSION/$VERSION/" -i $BUILDDIR/DEBIAN/control

echo "Download binary from architecture."
wget -O ./OpenRSAT $URL/OpenRSAT-linux-$ARCH

echo "Move downloaded binary."
mv ./OpenRSAT $BUILDDIR/opt/openrsat/
echo "Copy icon."
cp $ICON_PATH $BUILDDIR/opt/openrsat/OpenRSAT.png
echo "Copy desktop file."
cp ./OpenRSAT.desktop $BUILDDIR/usr/share/applications/

echo "Set permissions to binary, icon and desktop files."
chmod 755 $BUILDDIR/opt/openrsat/OpenRSAT
chmod 755 $BUILDDIR/opt/openrsat/OpenRSAT.png
chmod 755 $BUILDDIR/usr/share/applications/OpenRSAT.desktop

echo "Build deb file."
dpkg-deb --build $BUILDDIR OpenRSAT-${DEBARCH}-${VERSION}.deb

echo "Remove build folder."
rm -rf $BUILDDIR