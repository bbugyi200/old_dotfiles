"""Plot base objects and some useful standard plots for disipyl.

-----------------------------------------------------------------------------
(c) Copyright by Paul M. Magwene, 2000-2002  (mailto:paul.magwene@yale.edu)

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
##  $Id: plots.py,v 1.2 2002/04/23 12:40:52 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.2 $"
__credits__ = ""

#------------------------------------------------------------------------------
try:
    import Numeric as Num
except ImportError:
    import utilities
    Num = utilities.Num
import math

#------------------------------------------------------------------------------
import utilities, pxdislin, pxdislin3D


#------------------------------------------------------------------------------
# Color and symbol loops

# We can use this combined color/index list because the color function
# is smart enough to use the correct call to dislin

colorlist = ['fore','red','green','blue','cyan','orange']
colorlist += range(100,255,25)
colorloop = utilities.Loop(colorlist)
symbollist = ['fill circle', 'fill square', 'fill triangle', 'fill diamond',\
         'circle', 'square', 'triangle', 'cross', 'diamond', 'star']
symbolloop = utilities.Loop(symbollist)


#------------------------------------------------------------------------------
def applyCycle(plot, option, cycle):
    """Applies a cycle to the dataobjects (not objects) of a plot."""
    ct = 0
    for obj in plot.dataobjects:
        obj.__setattr__(option, cycle[ct])
        ct = ct + 1

#------------------------------------------------------------------------------
# Axis calculating function

def auto_axis(xlist):
    """Estimates generally nice axis values for a single axis."""
    minX, maxX = min(xlist), max(xlist)
    dx = maxX-minX
    xmin = minX - 0.05*dx
    xmax = maxX + 0.05*dx
    dx = xmax - xmin

    # Handle special cases where total range is very small
    if dx < 1e-10:
        xmax = xmin + 0.1
        dx = 0.1

    xstep = 0
    if dx >= 1.0:
        xstep = round(dx/5.,0)
        if dx/5. < 1:
            xstep = round(dx/5.,1)
        xstart = round(min(xlist),0)
        if xstart < xmin:
            xstart = round(xmin,1) + (xstep/2)
    else:
        xv = 0.25
        n = 1
        if dx < 0.1:
            m,e = math.frexp(dx)
            n = -math.floor(math.sqrt(abs(e)))
        xstep = xv * math.pow(10,n-1)
        xstart = min(xlist)

    class AxisInfo:
        pass

    ai = AxisInfo()
    ai.min = xmin
    ai.max = xmax
    ai.start = xstart
    ai.step = xstep
    return ai



def auto_axes2D(xlist, ylist, squared=0, axes = None):
    """Utility function for calculating nice looking 2-D axis options."""

    aix = auto_axis(xlist)
    aiy = auto_axis(ylist)

    rangeX = abs(aix.max - aix.min)
    rangeY = abs(aiy.max - aiy.min)

    # Setup min/max so both x and y have same range (gives squared coordinates)
    if squared:
        maxrange = max(rangeX,rangeY)
        aix.max = aix.min + maxrange
        aiy.max = aiy.min + maxrange
        if rangeX > rangeY:
            aiy.step = aix.step
        else:
            aix.step = aiy.step

    class AxisInfo:
        pass

    ai = AxisInfo()
    ai.xmin = aix.min
    ai.xmax = aix.max
    ai.ymin = aiy.min
    ai.ymax = aiy.max
    ai.xstep = aix.step
    ai.ystep = aiy.step
    ai.xstart = aix.start
    ai.ystart = aiy.start
    if axes:
       axes.xaxis(min = ai.xmin, max = ai.xmax,
                        tickstart = ai.xstart, tickstep = ai.xstep)
       axes.yaxis(min = ai.ymin, max = ai.ymax,
                        tickstart = ai.ystart, tickstep = ai.ystep)
    return ai


def auto_axes3D(xlist, ylist, zlist, axes = None):
    """Utility function for calculating decent looking default 3-D axis options."""
    ai = auto_axes2D(xlist, ylist, axes = axes)
    aiz = auto_axis(zlist)

    ai.zmin = aiz.min
    ai.zmax = aiz.max
    ai.zstep = aiz.step
    ai.zstart = aiz.start
    if axes:
        axes.zaxis(min = ai.zmin, max = ai.zmax,
                        tickstart = ai.zstart, tickstep = ai.zstep)

    xctr = ai.xmin + (ai.xmax - ai.xmin)/2.
    yctr = ai.ymin + (ai.ymax - ai.ymin)/2.
    zctr = ai.zmin + (ai.zmax - ai.zmin)/2.

    ai.focus = (xctr, yctr, zctr)

    return ai

#------------------------------------------------------------------------------
# Plot2D

class Plot2D(pxdislin.PlotObject):
    """Base class for two-dimensional plots."""
    def __init__(self, **keywords):
        pxdislin.PlotObject.__init__(self, **keywords)
        self.setup_plot()

    def set_defaults(self):
        pxdislin.PlotObject.set_defaults(self)

    def setup_plot(self):
        self.set_axes()
        self.set_dataobjects()

    def set_axes(self):
        self.axes = pxdislin.AxisSystem2D(pxdislin.Axis(), pxdislin.Axis())

    def set_dataobjects(self):
        pass

    def setDataOptions(self, n=None, **keywords):
        if n is not None:
            self.dataobjects[n](**keywords)
        else:
            for obj in self.dataobjects:
                obj(**keywords)

# setup alias
Plot2D.refresh = Plot2D.setup_plot


#------------------------------------------------------------------------------

class ScatterPlot(Plot2D):
    """Simple scatter plot from sequences of x and y values.

    Comments:
        * Symbols are represented as a SymbolGroup
    """
    def __init__(self, xlist, ylist, labels=None, symboloptions={}, **keywords):
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            labels = labels,
            symboloptions = symboloptions,
        )
        Plot2D.__init__(self, **keywords)



    def set_defaults(self):
        Plot2D.set_defaults(self)
        self.setoptions(
            symbols = None,
        )


    def set_dataobjects(self):
        if self.labels is not None:
            labels = [str(i) for i in self.labels]
            g = pxdislin.LabeledSymbolGroup(self.xlist, self.ylist,
                                            labels, **self.symboloptions)
        else:
            g = pxdislin.SymbolGroup(self.xlist, self.ylist, **self.symboloptions)
        self.add(g)
        self(symbols = self.dataobjects[0])

    def set_axes(self):
        Plot2D.set_axes(self)
        # Setup autoscaling for this data
        ai = auto_axes2D(self.xlist, self.ylist, squared = 0, axes = self.axes)


    def cleanup(self):
        self.axes(autoscaling=0)    # turn of autoscaling after drawing

#------------------------------------------------------------------------------

class VectorPlot(Plot2D):
    """Simple two-dimensional vector plot."""
    def __init__(self, xstarts, ystarts, xends, yends, **keywords):
        self.setdata(
            xstarts = xstarts, xends = xends,
            ystarts = ystarts, yends = yends,
        )
        Plot2D.__init__(self, **keywords)

    def set_axes(self):
        Plot2D.set_axes(self)
        allx = list(self.xstarts) + list(self.xends)
        ally = list(self.ystarts) + list(self.yends)
        ai = auto_axes2D(allx, ally, squared = 1, axes = self.axes)

    def set_dataobjects(self):
        self.vecs =[pxdislin.Vector(i[0],i[1],i[2],i[3]) for i in \
                zip(self.xstarts, self.ystarts, self.xends, self.yends)]
        self.add(*self.vecs)


#------------------------------------------------------------------------------

class CurvePlot(Plot2D):
    """Two-dimensional Curve plot."""
    def __init__(self, xlist, ylist, curveoptions={}, **keywords):
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            curveoptions = curveoptions,
        )
        Plot2D.__init__(self, **keywords)

    def set_defaults(self):
        Plot2D.set_defaults(self)
        self.setoptions(
            curve = None,
        )

    def set_axes(self):
        Plot2D.set_axes(self)
        ai = auto_axes2D(self.xlist, self.ylist, squared = 0, axes = self.axes)

    def set_dataobjects(self):
        self.add(pxdislin.Curve(self.xlist,self.ylist, **self.curveoptions))
        self(curve = self.dataobjects[0])



class FunctionPlot(CurvePlot):
    """Two-dimensional plot representing y=f(x)."""
    def __init__(self, func, first, last, step, **keywords):
        xlist = list(Num.arange(first, last, step))
        ylist = [func(x) for x in xlist]
        self.setdata(
            xlist = xlist,
            ylist = ylist,
        )
        CurvePlot.__init__(self, self.xlist, self.ylist, **keywords)


#------------------------------------------------------------------------------
class HistogramPlot(Plot2D):
    """Constructs histogram plot from bin values and heights.

    Comments:
        * Uses reasonable starting axis values but may have to be adjusted.
        * The bin values represent the ticks over which the bars are centered.
    """
    def __init__(self, bins, heights, baroptions={}, **keywords):
        self.setdata(
            bins = bins,
            heights = heights,
            baroptions = baroptions,
        )
        Plot2D.__init__(self, **keywords)

    def set_defaults(self):
        Plot2D.set_defaults(self)
        self.setoptions(
            bars = None,
        )


    def set_dataobjects(self):
        bars = pxdislin.HistoBars(self.bins, self.heights, **self.baroptions)
        self.add(bars)
        self(bars = bars)

    def set_axes(self):
        Plot2D.set_axes(self)
        aix = auto_axis(self.bins)
        aiy = auto_axis(self.heights)
        minX, maxX = min(self.bins), max(self.bins)
        maxY = max(self.heights)
        self.axes.xaxis(min = aix.min, max=aix.max, tickstart = minX, tickstep = aix.step)
        self.axes.yaxis(min = 0, max= aiy.max, tickstart= 0, tickstep = aiy.step)




#------------------------------------------------------------------------------
# Plot3D

class Plot3D(Plot2D):
    """Base class for three-dimensional plots."""
    def set_axes(self):
        self.axes = pxdislin3D.AxisSystem3D(pxdislin.Axis(),pxdislin.Axis(),pxdislin.Axis())
        self.axes(viewtype='angle')
        self.axes(viewpoint = (150.,30.,7.5)) # Reasonable starting view
        self.axes(clipped = 0)

    #These rotation functions only work if using the 'angle' viewtype
    def rotate_left(self, val=10):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0]-val,vp[1],vp[2]))

    def rotate_right(self, val=10):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0]+val,vp[1],vp[2]))

    def rotate_up(self, val=10):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0],vp[1]-val,vp[2]))

    def rotate_down(self, val=10):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0],vp[1]+val,vp[2]))

    def zoom_in(self, val=1):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0],vp[1],vp[2]-val))

    def zoom_out(self, val=1):
        vp = self.axes.viewpoint
        self.axes(viewpoint = (vp[0],vp[1],vp[2]+val))

    # tilt works regardless of viewtype

    def tilt_left(self, val=5):
        rot = self.axes.camerarotation
        self.axes(camerarotation = rot + val)

    def tilt_right(self, val=5):
        rot = self.axes.camerarotation
        self.axes(camerarotation = rot - val)



#------------------------------------------------------------------------------
# Common 3D Plots

class CurvePlot3D(Plot3D):
    """Plots 3D scatter of points."""
    def __init__(self, xlist, ylist, zlist, curveoptions={}, **keywords):
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
            curveoptions = curveoptions,
        )
        Plot3D.__init__(self, **keywords)

    def set_defaults(self):
        Plot3D.set_defaults(self)
        self.setoptions(
            curve = None,
        )


    def set_dataobjects(self):
        g = pxdislin3D.Curve3D(self.xlist, self.ylist, self.zlist, **self.curveoptions)
        self.add(g)
        self(curve = self.dataobjects[0])

    def set_axes(self):
        Plot3D.set_axes(self)
        ai = auto_axes3D(self.xlist, self.ylist, self.zlist, axes = self.axes)
        self.axes(focuspoint = ai.focus)

    def cleanup(self):
        self.axes(autoscaling=0)    # turn off autoscaling after drawing




class ScatterPlot3D(CurvePlot3D):
    """Plots 3D scatter of points."""
    def __init__(self, xlist, ylist, zlist, symboloptions={}, **keywords):
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
            symboloptions = symboloptions,
        )
        Plot3D.__init__(self, **keywords)

    def set_defaults(self):
        Plot3D.set_defaults(self)
        self.setoptions(
            symbols = None,
        )


    def set_dataobjects(self):
        g = pxdislin3D.Symbols3D(self.xlist, self.ylist, self.zlist, **self.symboloptions)
        self.add(g)
        self(symbols = self.dataobjects[0])




class SurfacePlot(Plot3D):
    """Plots 3D surface representing f(x,y)."""
    def __init__(self, function, xstart, xstop, xstep, ystart, ystop, ystep, **keywords):
        self.setdata(
            function = function,
            xstart = xstart, xstop = xstop, xstep = xstep,
            ystart = ystart, ystop = ystop, ystep = ystep,
        )
        Plot3D.__init__(self, **keywords)

    def set_defaults(self):
        Plot3D.set_defaults(self)
        self.setoptions(
            surface = None,
        )


    def set_axes(self):
        Plot3D.set_axes(self)
        xlist = Num.arange(self.xstart, self.xstop+1, self.xstep)
        ylist = Num.arange(self.ystart, self.ystop+1, self.ystep)
        zlist = utilities.zcalculator(self.function, xlist, ylist)
        ai = auto_axes3D(xlist, ylist, zlist, axes = self.axes)
        self.axes(focustype = 'user', focuspoint = ai.focus)


    def set_dataobjects(self):
        surface = pxdislin3D.FunctionSurface(self.function, self.xstep, self.ystep, 2, 2)
        self.add(surface)
        self(surface = surface)



#------------------------------------------------------------------------------
##  $Log: plots.py,v $
##  Revision 1.2  2002/04/23 12:40:52  pmagwene
##  Fixed a bug in FunctionPlot (submitted by Tomas Ficko)
##
##  Revision 1.1.1.1  2002/04/11 15:18:56  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.4  2002/03/09 21:34:35  pmagwene
##  added quickplots
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
##  Revision 1.15  2001/04/20 02:53:53  pmagwene
##  minor tweaks
##
##  Revision 1.14  2001/04/17 20:05:09  pmagwene
##  *** empty log message ***
##
##  Revision 1.13  2001/04/05 14:44:20  pmagwene
##  Messed around with __setattr__ in base Object
##  Added miscplots.py - for useful plots that don't deserver to be
##   in plots.py (which should be rserved for the most basic plots)
##
##  Revision 1.12  2001/03/27 03:00:00  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.11  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
##  Revision 1.10  2001/03/25 20:19:57  pmagwene
##  Modified demos to return Canvas rather than plot objects
##  Renamed tkdislin -> tkdisipyl
##  Fleshed out tkdisipyl
##  Added cool 2D and 3D demos to tkdisipyl
##
##  Revision 1.9  2001/03/23 19:08:30  pmagwene
##  *** empty log message ***
##
##  Revision 1.8  2001/03/23 00:22:51  pmagwene
##  Some tweaknig of AxisSystem2D, Axis, and related functions to give better defaults.
##  Started mlabplot -- a set of functions with similar functionality to their counterparts in MATLAB.
##
##  Revision 1.7  2001/03/21 18:32:15  pmagwene
##  Fixed up HistogramPlot
##
##  Revision 1.6  2001/03/21 17:41:57  pmagwene
##  Cleanup and doc strings.  Minor fixes.
##
##  Revision 1.5  2001/03/20 03:24:44  pmagwene
##  Added module for automatic documentation (disipyldoc.py)
##  Added doc strings to most objects.
##
##  Revision 1.4  2001/03/19 08:53:00  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.3  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##
