""" Quick versions of disipyl plots

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

##  $Id: quickplots.py,v 1.1.1.1 2002/04/11 15:19:07 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""


#----------------------------------------------------------------------------
import utilities, pxdislin, plots, contours
from plots import *

hasnumeric = 1
try:
    import Numeric
except ImportError:
    hasnumeric = 0

#----------------------------------------------------------------------------

def plot(x, y, draw=1, **keywords):
    """Given two data sequences (of equal length), draw curve based on pts.

    Comments:
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    plot = plots.CurvePlot(x,y, **keywords)
    if draw:
        plot.draw()
    return plot



def scatter(x,y, draw=1, **keywords):
    """Given two data sequences (of equal length), draw scatter plot based on pts.

    Comments:
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    plot = plots.ScatterPlot(x, y, **keywords)
    if draw:
        plot.draw()
    return plot



def histogram(data, nbins=8, draw=1, **keywords):
    """Given data sequence (list, array, etc), automatically generates a histogram.

    Comments:
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    if not hasnumeric:
        print "quickplots.histogram requires the Numeric module"
        return None

    histo = utilities.Histogram(data, nbins)
    plot = plots.HistogramPlot(histo[:,0], histo[:,1], **keywords)
    if draw:
        plot.draw()
    return plot



def pseudocolor(M, draw=1):
    """Draws a psuedo-color representation of a matrix.

    Comments:
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    if not hasnumeric:
        print "quickplots.pseudocolor requires the Numeric module"
        return None

    nr, nc = M.shape
    plot = contours.ColorPlot()
    ai = plots.auto_axis(Num.ravel(M))
    plot.axes.zaxis(min = ai.min, max = ai.max,
                    tickstart = ai.start, tickstep = ai.step)
    plot.axes(autoresolution = (nr,nc))
    clrmatrix = contours.ColorMatrix(M, nr, nc)
    plot.add(clrmatrix)

    if draw:
        plot.draw()
    return plot


def surface(x, y, z, draw=1, **keywords):
    """Draws a surface based on x,y coordinates and z values (len(z) should be len(x)*len(y).

    Comments:
        * The len of z should be len(x)*len(y)
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """

    plot = plots.Plot3D(**keywords)
    ai = plots.auto_axes3D(x,y,z)
    plot.axes.xaxis(min = ai.xmin, max = ai.xmax,
                    tickstart = ai.xstart, tickstep = ai.xstep)
    plot.axes.yaxis(min = ai.ymin, max = ai.ymax,
                    tickstart = ai.ystart, tickstep = ai.ystep)
    plot.axes.zaxis(min = ai.zmin, max = ai.zmax,
                    tickstart = ai.zstart, tickstep = ai.zstep)

    plot.axes(focustype='user', focuspoint=ai.focus)
    surface = pxdislin3D.IrregularSurface(x,y,z)
    plot.add(surface)

    if draw:
        plot.draw()
    return plot



def contour(f, rangex, rangey, zinterval, draw=1):
    """Draws a contour plot of f(x,y) over the given ranges, contours drawn at zinterval.

    Comments:
        * f is a function which takes two arguments and returns a single numeric value
        * rangex and rangey are (min, max) tuples
        * default stepsize = (max - min)/10
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    xdiff = (rangex[1] - rangex[0])
    xstep = xdiff/10.
    ydiff = (rangey[1] - rangey[0])
    ystep = ydiff/10.


    plot = plots.Plot2D()
    ai = auto_axes2D(rangex, rangey, squared = 0)
    plot.axes.xaxis(min = rangex[0], max = rangex[1],
                    tickstart = ai.xstart, tickstep = ai.xstep)
    plot.axes.yaxis(min = rangey[0], max = rangey[1],
                    tickstart = ai.ystart, tickstep = ai.ystep)
    plot.axes.xaxis(name='X-Axis')
    plot.axes.yaxis(name='Y-Axis')

    cntrs = contours.FunctionContours(f, rangex[0], rangex[1]+xstep, xstep,
                                         rangey[0], rangey[1]+ystep, ystep,
                                         zstep=zinterval,
                                         colored=1, colorstart=30, colorstep=30,
                                         labeltype='float')
    plot.add(cntrs)

    if draw:
        plot.draw()
    return plot

#----------------------------------------------------------------------------

def test():
    if not hasnumeric:
        print "quickplots.test requires Numeric"
        return None

    import Numeric as Num, random, math

    x = Num.arange(100, typecode=Num.Float)
    plot(x, Num.sin(x/5))

    y = [random.normalvariate(i, 4) for i in x]
    scatter(x,y)

    y = [random.normalvariate(0, 0.5) for i in range(1000)]
    histogram(y, nbins=15)

    z = Num.array([math.cos(i/(math.pi*2))*math.sin(j/(math.pi*2)) for i in x for j in x])
    z.shape = (len(x), len(x))
    pseudocolor(z)

    x = range(10)
    y = range(10)
    z = [i**2 - j**2 for i in x for j in y]
    surface(x,y,z)

    def f(x,y):
        return abs(x**2-1)**0.5 + abs(y**2-1)**0.5

    contour(f, (0,10), (0,10), 1)



if __name__ == "__main__":
    test()
