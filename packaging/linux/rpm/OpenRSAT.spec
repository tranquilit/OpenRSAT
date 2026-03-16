%define buildroot ./builddir

Name:		OpenRSAT
Version:	%{_version}
Release:	1
Summary:	OpenRSAT made by Tranquil IT
BuildArch:	%{_target_cpu}

License:	GPL
URL:		https://www.tranquil.it

Requires:	bash, gtk2

%description

%install
set -ex
mkdir -p %{buildroot}/opt/openrsat
mkdir -p %{buildroot}/usr/share/applications
mkdir -p %{buildroot}/usr/bin

rsync -aP OpenRSAT %{buildroot}/opt/openrsat/
rsync -aP OpenRSAT.png %{buildroot}/opt/openrsat/
rsync -aP OpenRSAT.desktop %{buildroot}/usr/share/applications/

%files

%attr(755,root,root)/opt/openrsat/
%attr(755,root,root)/opt/openrsat/OpenRSAT
%attr(755,root,root)/opt/openrsat/OpenRSAT.png
%attr(755,root,root)/usr/share/applications/OpenRSAT.desktop

%post
rm -rf /usr/bin/OpenRSAT
ln -s /opt/openrsat/OpenRSAT /usr/bin/OpenRSAT