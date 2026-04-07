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

Requires:	bash, gtk2

%description

%install
rm -rf %{buildroot}

set -ex
mkdir -p %{buildroot}/opt/openrsat
mkdir -p %{buildroot}/usr/share/applications
mkdir -p %{buildroot}/usr/bin

install -Dpm0755 %{SOURCE0} %{buildroot}/opt/openrsat/OpenRSAT
install -Dpm0755 %{SOURCE1} %{buildroot}/opt/openrsat/OpenRSAT.png
install -Dpm0755 %{SOURCE2} %{buildroot}/usr/share/applications/OpenRSAT.desktop

%files
/opt/openrsat/OpenRSAT
/opt/openrsat/OpenRSAT.png
/usr/share/applications/OpenRSAT.desktop

%post
rm -rf /usr/bin/OpenRSAT
ln -s /opt/openrsat/OpenRSAT /usr/bin/OpenRSAT