#!/bin/bash

VERSION="0.4.30"
URL="https://wapt.tranquil.it/store/fr/wapt/tis-openrsat_$VERSION.2_linux_PROD.wapt"
BUILDDIR=$PWD/builddir

rm -rf *.deb $BUILDDIR

mkdir -p $BUILDDIR/DEBIAN

cp ./control $BUILDDIR/DEBIAN
cp ./postinst $BUILDDIR/DEBIAN

chmod 755 $BUILDDIR/DEBIAN/postinst

sed "s/VERSION/$VERSION/" -i $BUILDDIR/DEBIAN/control

mkdir -p $BUILDDIR/{opt/openrsat/,usr/share/applications}

wget -O tis-openrsat.zip $URL
unzip -d tis-openrsat tis-openrsat.zip
rm -rf tis-openrsat.zip

cp tis-openrsat/bin/linux-x64/OpenRSAT $BUILDDIR/opt/openrsat/
cp tis-openrsat/WAPT/icon.png $BUILDDIR/opt/openrsat/OpenRSAT.png
cp tis-openrsat/OpenRSAT.desktop $BUILDDIR/usr/share/applications/
rm -rf tis-openrsat

chmod 755 $BUILDDIR/opt/openrsat/OpenRSAT
chmod 755 $BUILDDIR/opt/openrsast/OpenRSAT.png
chmod 755 $BUILDDIR/usr/share/applications/OpenRSAT.desktop

dpkg-deb --build $BUILDDIR OpenRSAT-${VERSION}.deb

rm -rf $BUILDDIR
