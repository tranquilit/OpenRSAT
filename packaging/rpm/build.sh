#/bin/sh

set -ex
BUILDDIR=$PWD/builddir

echo "Setting up environment"
rm -rf $BUILDDIR BUILD RPMS SRPMS BUILDROOT
mkdir -p ./{BUILD,RPMS}

VERSION="0.4.30"
URL="https://wapt.tranquil.it/store/fr/wapt/tis-openrsat_$VERSION.2_linux_PROD.wapt"

echo "Download from $URL"

wget -O ./BUILD/tis-openrsat.zip $URL
unzip -d ./BUILD/ ./BUILD/tis-openrsat.zip
cp BUILD/bin/linux-x64/OpenRSAT ./BUILD/
cp BUILD/WAPT/icon.png ./BUILD/OpenRSAT.png

echo "Build rpm"
rpmbuild -bb --buildroot $BUILDDIR --define "_version $VERSION" --clean ./OpenRSAT.spec

echo "Retrieve rpm"
mv RPMS/*/*.rpm .

echo "Remove builddir"
rm -rf $BUILDDIR BUILD RPMS SRPMS BUILDROOT SOURCES SPECS

echo "Success"
