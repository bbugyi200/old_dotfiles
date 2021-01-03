"""Utility functions for disipyl.

-----------------------------------------------------------------------------
(c) Copyright by Paul M. Magwene, 2002  (mailto:paul.magwene@yale.edu)

    Permission to use, copy, modify, and distribute this software and its
    documentation for any purpose and without fee or royalty is hereby granted,
    provided that the above copyright notice appear in all copies and that
    both that copyright notice and this permission notice appear in
    supporting documentation or portions thereof, including modifications,
    that you make.

    THE AUTHOR PAUL M. MAGWENE DISCLAIMS ALL WARRANTIES WITH REGARD TO
    THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
    FITNESS, IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL,
    INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING
    FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
    NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
    WITH THE USE OR PERFORMANCE OF THIS SOFTWARE !
-----------------------------------------------------------------------------
"""

#------------------------------------------------------------------------------
##  $Id: utilities.py,v 1.1.1.1 2002/04/11 15:19:10 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""

#------------------------------------------------------------------------------
import types, sys, copy, os, os.path, cPickle, pickle
import UserList

#---------------------------------------------------------------------------
# If numpy isn't available we will provide an arange function

def arange(start, stop=None, step=None):
    if stop is None:
        stop = start
        start = 0
    if step is None:
        step = 1
    if start > stop and step < 0:
        return decarange(start, stop, step)
    last = stop - step
    vals = []
    total = start
    while total <= last:
        vals.append(total)
        total += step
    return vals

def decarange(start, stop, step):
    last = stop - step
    vals = []
    total = start
    while total >= last:
        vals.append(total)
        total += step
    return vals

#------------------------------------------------------------------------------
try:
    import numpy as Num
except ImportError:
    class NumProxy:
        pass
    Num = NumProxy()
    Num.arange = numpy.arange    

#------------------------------------------------------------------------------
# Pickle functions

def pickleObject(obj,fname):
    """Routine for pickling a disipyl Object."""
    f = open(os.path.expanduser(fname),'wb')
    cPickle.dump(obj,f,1)

def unpickleObject(fname):
    """Routine for loading a pickled disipyl object. Returns None on failure"""
    f = open(os.path.expanduser(fname),'rb')
    try:
        obj = cPickle.load(f)
    except:
        f.close()
        raise
    return obj
#    try:
#        obj = cPickle.load(f)
#    except cPickle.UnpicklingError:
#        return None
#    return obj   

#------------------------------------------------------------------------------

def isclass(obj):
    """Return true if the object is a class."""
    return type(obj) is types.ClassType


#------------------------------------------------------------------------------

def allExist(*args):
    """If all arguments are not None, return 1, else return 0"""
    for each in args:
        if each is None:
            return 0
    return 1   
    
    
#------------------------------------------------------------------------------
def collapse(inlist, type=type, listtype=types.ListType, \
            integers = xrange(sys.maxint), endoflist=IndexError):
    """Non destructively flatten a list hierarchy to a single level. """
    outlist = copy.copy(inlist)
    try:
        for ind in integers :
            while type(outlist[ind]) is listtype:
                outlist[ind:ind+1] = outlist[ind]
    except endoflist:
        return outlist   
    
#------------------------------------------------------------------------------
    

class Loop(UserList.UserList):
    """Circular sequence object.
    
    Indexing past last value loops back to the beginning.
"""
    def __getitem__(self,key):
        rlen = len(self.data)
        if key >= rlen or key < 0:
            return self.data[ (key % rlen) ]
        else:
            return self.data[key]
            
#------------------------------------------------------------------------------

def zcalculator(func, xlist, ylist):
    """Calcates z=func(x,y) for values of xlist,ylist."""
    return [func(i,j) for i in xlist for j in ylist]
    
#------------------------------------------------------------------------------


# Histograms.
#
# Written by Konrad Hinsen <hinsen@cnrs-orleans.fr>
# last revision: 1999-9-24
#


class Histogram:
    """Histogram in one variable
    
    From Konrad Hinsen's ScientificPython Package.

    Constructor: Histogram(|data|, |bins|, |range|=None)

    Arguments:

    |data| -- a sequence of data points

    |bins| -- the number of bins into which the data is to be sorted

    |range| -- a tuple of two values, specifying the lower and
               the upper end of the interval spanned by the bins.
               Any data point outside this interval will be ignored.
               If no range is given, the smallest and largest
               data values are used to define the interval.

    The bin index and the number of points in a bin can be obtained by
    indexing the histogram with the bin number. Application of len()
    yields the number of bins. A histogram thus behaves like a
    sequence of bin index - bin count pairs.
    """

    def __init__(self, data, nbins, range=None):
        if range is None:
            self.min = Numeric.minimum.reduce(data)
            self.max = Numeric.maximum.reduce(data)
        else:
            self.min, self.max = range
        self.min = self.min+0.
        self.max = self.max+0.
        self.bin_width = (self.max-self.min)/nbins
        self.array = Numeric.zeros((nbins, 2), Numeric.Float)
        self.array[:, 0] = self.min + self.bin_width*(Numeric.arange(nbins)+0.5)
        self.addData(data)

    def __len__(self):
        return self.array.shape[0]

    def __getitem__(self, index):
        return self.array[index]

    def __getslice__(self, first, last):
        return self.array[first:last]

    def addData(self, data):
        """Add the values in |data| (a sequence of numbers) to the
        originally supplied data. Note that this does not affect the
        default range of the histogram, which is fixed when the
        histogram is created.
        """
        n = (len(data)+999)/1000
        for i in range(n):
            self._addData(data[1000*i:1000*(i+1)])

    def _addData(self, data):
        data = Numeric.array(data, Numeric.Float)
        data = Numeric.repeat(data, Numeric.logical_and(Numeric.less_equal(data, self.max),
                                            Numeric.greater_equal(data, self.min)))
        data = Numeric.floor((data - self.min)/self.bin_width).astype(Numeric.Int)
        nbins = self.array.shape[0]
        histo = Numeric.add.reduce(Numeric.equal(Numeric.arange(nbins)[:,Numeric.NewAxis], data), -1)
        histo[-1] = histo[-1] + Numeric.add.reduce(Numeric.equal(nbins, data))
        self.array[:, 1] =  self.array[:, 1] + histo

    def normalize(self, norm=1.):
        "Scales all counts by the same factor such that their sum is |norm|."
        self.array[:, 1] = norm*self.array[:, 1]/Numeric.add.reduce(self.array[:, 1])

    def normalizeArea(self, norm=1.):
        """Scales all counts by the same factor such that the area under
        the histogram is |norm|."""
        self.normalize(1./self.bin_width)



#------------------------------------------------------------------------------
##  $Log: utilities.py,v $
##  Revision 1.1.1.1  2002/04/11 15:19:10  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.3  2002/03/09 01:12:37  pmagwene
##  removed disipyldoc
##
##  Revision 1.2  2002/01/29 02:51:32  pmagwene
##  changed to __options__ mechanism
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.10  2001/04/20 02:53:53  pmagwene
##  minor tweaks
##
##  Revision 1.9  2001/03/28 14:42:14  pmagwene
##  Cleanup and bug-fixes.
##
##  Revision 1.8  2001/03/27 02:48:44  pmagwene
##  fixed up pickling
##
##  Revision 1.7  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
##  Revision 1.6  2001/03/20 03:24:45  pmagwene
##  Added module for automatic documentation (disipyldoc.py)
##  Added doc strings to most objects.
##
##  Revision 1.5  2001/03/19 22:58:07  pmagwene
##  Tinkering with demos, minor modifications of pxdislin and utilities
##
##  Revision 1.4  2001/03/19 08:53:01  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.3  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##
