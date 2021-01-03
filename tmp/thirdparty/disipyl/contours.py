"""A module for creating contour plots and 3-D color plots.

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
##  $Id: contours.py,v 1.1.1.1 2002/04/11 15:18:51 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""

#------------------------------------------------------------------------------
import types
try:
    import Numeric as Num
except ImportError:
    import utilities
    Num = utilities.Num
#------------------------------------------------------------------------------

import pydislin as pdis, pxdislin, utilities, plots
from utilities import allExist, zcalculator
from pxdislin import DislinException, Axis, Object, DrawObject, DrawGroup,\
                     AxisSystem2D
                     
#------------------------------------------------------------------------------

clridxloop = utilities.Loop(range(256))                     
                     
#------------------------------------------------------------------------------
# Objects for contour plots

class ContourObject(DrawObject):
    """Base class for contour objects."""
    
    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            labelcolor = -1,
            labeldigits = 1,
            labeldistance = 500,
            labelgap = 0.5,
            labelstring = '',
            labeltype = 'none',
            linestyle = 'solid',
            linewidth = 1,
        )


    def configure(self):
        DrawObject.configure(self)
        pdis.setContourLabels(self.labeltype, self.labeldigits)
        pdis.setContourLabelDistance(self.labeldistance)
        pdis.setContourLabelColor(self.labelcolor)
        pdis.setContourLabelString(self.labelstring)
        pdis.setContourLabelGap(self.labelgap)
        pdis.setLineStyle(self.linestyle)
        pdis.setLineWidth(self.linewidth)
    
    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1)



class Contour(ContourObject):
    """Object representing a single contour.
    
    xlist, ylist -- lists of x,y coordinates
    zlist -- list or matrix with z-values corresponding to x,y coordinates
    """
    def __init__(self, xlist=[], ylist=[], zlist=[], level=None, **keywords):
        disfunc = pdis.drawContour
        ContourObject.__init__(self, disfunc, **keywords)    
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
            level = level,
        )
        
    def configure(self):
        ContourObject.configure(self)
        self.args = (self.xlist, self.ylist, self.zlist, self.level)



class ContourMatrix(ContourObject):
    """Object representing a single contour representation of a matrix.
    
    * Drawn using current X,Y-axis values.
    
    Arguments:
        * zmatrix: 2-D Numeric array.
        * level: level at which to draw contour
    """
    def __init__(self, zmatrix=None, level=None, **keywords):
        disfunc = pdis.drawMatrixContour
        ContourObject.__init__(self, disfunc, **keywords)    
        self.setdata(
            zmatrix = zmatrix,
            level = level
        )
        
    def configure(self):
        ContourObject.configure(self)
        nrows, ncols = self.zmatrix.shape
        self.args = (self.zmatrix, nrows, ncols, self.level)    



class ShadedContours(ContourObject):
    """Object representing shaded contours.
    
    Arguments:
        * xlist, ylist: sequences of x and y values over which function is
            calculated
        * zmatrix: 
    """
    def __init__(self, xlist=[], ylist=[], zmatrix=[], levels=None, **keywords):
        disfunc = pdis.drawShadedContours
        ContourObject.__init__(self, disfunc, **keywords)     
        self.setdata( 
            xlist = xlist,
            ylist = ylist,
            zmatrix = zmatrix,
            levels = levels,
        )
        
    def set_defaults(self):
        ContourObject.set_defaults(self)
        self.setoptions( 
            fillmode = 'cell',
        )


    def configure(self):
        ContourObject.configure(self)
        pdis.setContourFilling(self.fillmode)
        self.args = (self.xlist, self.ylist, self.zmatrix, self.levels)      
        

class FunctionContours(Object):
    """Object representing contours for a given function, f(x,y).
    
    Arguments:
        * func: function which accepted two arguments and returns single value
        * xstart, xstop, xstep: starting and ending values, and step size over 
            which to calculate contours.
        * ystart, ystop, ystep: see above.
        * zstep: contour interval.
    """
    def __init__(self, func=None, xstart=None, xstop=None, xstep=None,\
                                  ystart=None, ystop=None, ystep=None, zstep=None, **keywords):
        Object.__init__(self, **keywords)
        self.setdata(
            func = func,
            xstart = xstart, xstop = xstop, xstep = xstep,
            ystart = ystart, ystop = ystop, ystep = ystep,
            zstep = zstep,
            contours = None,
        )
        
        if allExist(func, xstart, xstop, xstep, ystart, ystop, ystep, zstep):        
            self.setup_data()
            self.setup_contours()

    def set_defaults(self):
        Object.set_defaults(self)
        self.setoptions( 
            labeltype = 'none',
            shaded = 0,
            colored = 0,
            colorstep = 25,
            colorstart = 25,
        )


    def setup_data(self):
        self.xlist = Num.arange(self.xstart, self.xstop, self.xstep)
        self.ylist = Num.arange(self.ystart, self.ystop, self.ystep)
        self.zlist = zcalculator(self.func, self.xlist, self.ylist)
        
    def setup_contours(self):
        zmin = min(self.zlist)
        zmax = max(self.zlist)
        self.levels = Num.arange(zmin, zmax, self.zstep)
        self.contours = []
        if self.shaded:
            self.contours = ShadedContours(self.xlist, self.ylist, self.zlist, self.levels)
            self.contours(labeltype = self.labeltype)
        else:
            clr = self.colorstart
            for level in self.levels:
                cntr = Contour(self.xlist, self.ylist, self.zlist, level)
                cntr(labeltype = self.labeltype)
                if self.colored:
                    cntr(color = clridxloop[clr])
                    clr += self.colorstep
                self.contours.append(cntr)       
            
    def draw(self):
        if not self.contours:
            raise DislinException('Error in FunctionContours object. Check arguments.')
        if self.shaded:
            if isinstance(self.contours, ShadedContours):
                self.contours.draw()
            else:
                self.setup_contours()
                self.draw()
        else:
            if type(self.contours) == types.ListType:
                for cntr in self.contours:
                    cntr.draw()
            else:
                self.setup_contours()
                self.draw()
    
 
#------------------------------------------------------------------------------
# Objects for 3D-color plots

class AxisSystemColor(AxisSystem2D):
    """Axis system object for 3D-color plots.
    
    Arguments:
        * xaxis, yaxis, zaxis: Axis objects.
    """
    def __init__(self, xaxis=None, yaxis=None, zaxis=None, **keywords):
        AxisSystem2D.__init__(self, xaxis, yaxis, **keywords)
        if zaxis:
            self.addZ(zaxis)
        else:
            self.addZ(Axis())     
            
    def addZ(self, zaxis):
        self.zaxis = zaxis
        if isinstance(self.zaxis, Axis):
            self.zaxis(whichaxis='Z')
        else:
            raise DislinException('Non-valid Axis specified')
        
    def set_defaults(self):
        AxisSystem2D.set_defaults(self)
        self.setoptions( 
            lengths = (None, None, None),
            barwidth = 85,
            barpositionx = 0,
            barpositiony = 0,
            resolution = (1,1),
            autoresolution = (None, None),
            suppresscolorbar = 0,
        )           
                               
        
    def configure(self):
        if allExist(*self.autoresolution):
            pdis.setAutoColorResolution(*self.autoresolution)
        AxisSystem2D.configure(self)
        if allExist(*self.lengths):
            pdis.setColorAxisLengths(*self.lengths)
        if self.suppresscolorbar:
            pdis.suppressColorBar()
        pdis.setColorBarWidth(self.barwidth)
        pdis.setColorBarPositionHoriz(self.barpositionx)
        pdis.setColorBarPositionVert(self.barpositiony)
        if not allExist(*self.autoresolution):
            pdis.setColorResolution(*self.resolution)

    def draw(self):            
        self.configure()
        try:
            if not self.xaxis or not isinstance(self.xaxis, Axis):
                raise DislinException('No x-axis specified or non-valid Axis object')
            if not self.yaxis or not isinstance(self.yaxis, Axis):
                raise DislinException('No y-axis specified or non-valid Axis object')
            if not self.zaxis or not isinstance(self.zaxis, Axis):
                raise DislinException('No z-axis specified or non-valid Axis object')
        except AttributeError:
            raise DislinException("Missing axis object for: %s" % str(self))            
        self.xaxis.draw()
        self.yaxis.draw()
        self.zaxis.draw()
        self.postaxis()
        self.displayit()
        self.postdisplay()
        
    def displayit(self):
        xaxis, yaxis, zaxis = self.xaxis, self.yaxis, self.zaxis
        xlow,xhigh,xfirst,xstep = xaxis.min, xaxis.max,xaxis.tickstart,xaxis.tickstep
        ylow,yhigh,yfirst,ystep = yaxis.min, yaxis.max,yaxis.tickstart,yaxis.tickstep 
        zlow,zhigh,zfirst,zstep = zaxis.min, zaxis.max,zaxis.tickstart,zaxis.tickstep 
        
        pdis.colorAxis3D(xlow,xhigh,xfirst,xstep,
                         ylow,yhigh,yfirst,ystep,
                         zlow,zhigh,zfirst,zstep)
                        
    def postdisplay(self):
        pass
        


class ColorMatrix(DrawObject):
    """Object representing 3D color matrix."""
    def __init__(self, matrix=None, nrows=None, ncols=None, **keywords):
        disfunc = pdis.drawColorMatrix
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata( 
            matrix = matrix,
            nrows = nrows,
            ncols = ncols,
        )

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            xinterp = 1,
            yinterp = 1,
        )
                       
    def configure(self):
        DrawObject.configure(self)        
        self.args = (self.matrix, self.nrows, self.ncols, self.xinterp, self.yinterp)



#------------------------------------------------------------------------------
# 3D-Color Plots

class ColorPlot(plots.Plot2D):
    """Base class for constructing 3D color plots."""
    def set_axes(self):
        self.axes = AxisSystemColor(Axis(),Axis(),Axis())



#------------------------------------------------------------------------------
##  $Log: contours.py,v $
##  Revision 1.1.1.1  2002/04/11 15:18:51  pmagwene
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
##  Revision 1.6  2001/04/05 14:44:19  pmagwene
##  Messed around with __setattr__ in base Object
##  Added miscplots.py - for useful plots that don't deserver to be
##   in plots.py (which should be rserved for the most basic plots)
##
##  Revision 1.5  2001/03/27 03:00:00  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.4  2001/03/20 03:24:44  pmagwene
##  Added module for automatic documentation (disipyldoc.py)
##  Added doc strings to most objects.
##
##  Revision 1.3  2001/03/19 08:53:00  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.2  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##
