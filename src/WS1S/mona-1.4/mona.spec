%define name mona
%define version 1.4
%define release 3

%define prefix /usr/local/bin

Summary: a decision procedure for the logics WS1S and WS2S
Name: %{name}
Version: %{version}
Release: %{release}
Copyright: GPL
Group: Development/Tools
URL: http://www.brics.dk/mona
Vendor: BRICS
Source: http:/www.brics.dk/mona/download/%{name}-%{version}-%{release}.tar.gz
BuildRoot: /tmp/mona-rpm
Prefix: %{prefix}
Packager: Anders Moeller <amoeller@brics.dk>

%description 
MONA is a tool that translates formulas in the logics WS1S or WS2S
into finite-state automata represented by BDDs.  The formulas may
express search patterns, temporal properties of reactive systems,
parse tree constraints, etc.  MONA also analyses the automaton
resulting from the compilation, and determines whether the formula is
valid and, if the formula is not valid, generates a counter-example.

%prep
%setup -q

%build
make

%install
mkdir -p $RPM_BUILD_ROOT%{prefix}
cp mona $RPM_BUILD_ROOT%{prefix}/

%clean
rm -rf $RPM_BUILD_ROOT

%files
%{prefix}/mona
