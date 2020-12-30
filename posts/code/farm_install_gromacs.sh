#!/bin/bash
CWD=$(pwd)  # establish the working directory

echo "Downloading cmake version 2.19.2"
wget https://github.com/Kitware/CMake/releases/download/v3.19.2/cmake-3.19.2-Linux-x86_64.sh
# Download cmake installer version 3.19
chmod 777 cmake-3.19.2-Linux-x86_64.sh
echo "Running cmake installer"
./cmake-3.19.2-Linux-x86_64.sh

CMAKE_EXE="$CWD/cmake-3.19.2-Linux-x86_64/bin/cmake"

echo "Downloading GROMACS 2021"
wget http://ftp.gromacs.org/pub/gromacs/gromacs-2021-rc1.tar.gz 
tar xfz gromacs-2021-rc1.tar.gz
cd gromacs-2021-rc1
mkdir build
cd build
echo "Building GROMACS 2021"
$CMAKE_EXE .. -DGMX_BUILD_OWN_FFTW=ON -DREGRESSIONTEST_DOWNLOAD=ON
make
make check

GMX_EXE="$(pwd)/bin/gmx"

echo "Installation is complete. The GROMACS exe is located at $GMX_EXE."