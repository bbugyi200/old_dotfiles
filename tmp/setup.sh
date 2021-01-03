#!/usr/bin/bash

set -e

# >>> egenix-mx-base
D=egenix-mx-base-3.2.9-py2.7_ucs4-linux-x86_64-prebuilt
wget https://downloads.egenix.com/python/"${D}".zip?python=2.7+ucs4+prebuilt2
unzip "${D}".zip\?python=2.7+ucs4+prebuilt2

pushd "${D}" || exit 1
python setup.py install
popd || exit 1

echo "from mx.DateTime import *; from mx.DateTime import _DT, _DTD" > "${VIRTUAL_ENV}"/lib/python2.7/site-packages/DateTime.py

rm -rf "${D}"

# >>> matplotlib
echo "from matplotlib.pylab import *; import matplotlib.pylab; __doc__ = matplotlib.pylab.__doc__" > "${VIRTUAL_ENV}"/lib/python2.7/site-packages/pylab.py
