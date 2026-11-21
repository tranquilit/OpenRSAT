%define buildroot ./builddir

Name:           OpenRSAT
Version:        %{_version}
Release:        1%{?dist}
Summary:        OpenRSAT made by Tranquil IT

BuildArch:	x86_64

License:        GPL
URL:            https://www.tranquil.it
Source0:        OpenRSAT
Source1:        OpenRSAT.png
Source2:        OpenRSAT.desktop
Source3:        OpenRSAT.en.po
Source4:        OpenRSAT.fr.po
Source5:        OpenRSAT.pl.po
Source6:        OpenRSAT.el.po

Requires:	bash, gtk2

%description

%install
rm -rf %{buildroot}

set -ex
mkdir -p %{buildroot}/opt/openrsat
mkdir -p %{buildroot}/usr/share/applications
mkdir -p %{buildroot}/usr/share/locale/{en,fr,pl,el}
mkdir -p %{buildroot}/usr/bin

install -Dpm0755 %{SOURCE0} %{buildroot}/opt/openrsat/OpenRSAT
install -Dpm0755 %{SOURCE1} %{buildroot}/opt/openrsat/OpenRSAT.png
install -Dpm0755 %{SOURCE2} %{buildroot}/usr/share/applications/OpenRSAT.desktop
install -Dpm0755 %{SOURCE3} %{buildroot}/usr/share/locale/en/OpenRSAT.po
install -Dpm0755 %{SOURCE4} %{buildroot}/usr/share/locale/fr/OpenRSAT.po
install -Dpm0755 %{SOURCE5} %{buildroot}/usr/share/locale/pl/OpenRSAT.po
install -Dpm0755 %{SOURCE6} %{buildroot}/usr/share/locale/el/OpenRSAT.po

%files
/opt/openrsat/OpenRSAT
/opt/openrsat/OpenRSAT.png
/usr/share/applications/OpenRSAT.desktop
/usr/share/locale/en/OpenRSAT.po
/usr/share/locale/fr/OpenRSAT.po
/usr/share/locale/pl/OpenRSAT.po
/usr/share/locale/el/OpenRSAT.po

%post
rm -rf /usr/bin/OpenRSAT
ln -s /opt/openrsat/OpenRSAT /usr/bin/OpenRSAT