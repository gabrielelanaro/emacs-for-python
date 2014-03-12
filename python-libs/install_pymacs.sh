#!/bin/sh

PYMACS_VERSION=0.25
PYTHON_VERSION=2.7

PYTHON=python$PYTHON_VERSION
EI=easy_install-$PYTHON_VERSION

#cleanup
rm -rf Pymacs* rope* site.* easy-install*

PYTHONPATH=./ $EI -z -d ./ rope ropemacs 
wget https://github.com/pinard/Pymacs/archive/master.tar.gz -O - | tar xzvf -

DNAME=$(ls -d *Pymacs*)
echo "Pymacs source directory: $DNAME"
cd $DNAME
$PYTHON pppp -C ppppconfig.py *.in contrib tests
$PYTHON setup.py sdist
PYMACS_VERSION=$( python -c "import Pymacs;print(Pymacs.version)" )
echo "Pymacs-version: $PYMACS_VERSION"
cp dist/Pymacs-$PYMACS_VERSION.tar.gz ../
cp -i pymacs.el ../../extensions/
cd ..
rm -rf $DNAME
PYTHONPATH=./ $EI -z -d ./ Pymacs-$PYMACS_VERSION.tar.gz
rm -f Pymacs-$PYMACS_VERSION.tar.gz