set -e
set -x

mkdir -p $TOOLS_DIR || true
cd $TOOLS_DIR

# If not already present, download the GNAT Community installer and the helper
# scripts to use it headless.
if ! [ -f $GNAT_INSTALLER ]
then
   if [ "$TRAVIS_OS_NAME" = "osx" ]
   then
      wget $GNAT_OSX_INSTALLER_URL -O $GNAT_INSTALLER
   else
      wget $GNAT_LINUX_INSTALLER_URL -O $GNAT_INSTALLER
   fi
fi
if ! [ -d gnat_community_install_script ]
then
   git clone https://github.com/AdaCore/gnat_community_install_script.git;
else
   (cd gnat_community_install_script && git pull)
fi

# If not already installed, install GNAT Community. The script does not work if
# the installation directory already exists, so remove it first.
if ! [ -f "$INSTALL_DIR/bin/gprbuild" ]
then
   rm -rf "$INSTALL_DIR"
   sh gnat_community_install_script/install_package.sh \
      "$GNAT_INSTALLER" "$INSTALL_DIR"
fi

# Log info about the toolchain we use
- which gprbuild && gprbuild --version

# Checkout gprbuild sources (or update them), and then build and install
# Libgpr.
if ! [ -d libgpr-src ]
then
   git clone https://github.com/AdaCore/gprbuild libgpr-src;
else
   (cd libgpr-src && git pull)
fi
(
   cd libgpr-src
   make libgpr.build
   make libgpr.install
)

# Install e3-testsuite to run GNATCOLL's testsuite
pip install git+https://github.com/AdaCore/e3-testsuite.git
