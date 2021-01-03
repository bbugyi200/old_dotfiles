""" Presents a MATLAB like interface for plotting.

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
##  $Id: mlabplots.py,v 1.1.1.1 2002/04/11 15:18:55 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""

#------------------------------------------------------------------------------

import Numeric as Num, LinearAlgebra as LA, RandomArray as RA, MLab

#------------------------------------------------------------------------------

from disipyl import pxdislin, plots, contours

#------------------------------------------------------------------------------

def plot(x, *args, **keywords):
    """Draws a 2D line plots.
    
    Comments:
        * Multiple curves can be specified eg. plot(xlist1, ylist1, xlist2, ylist2, etc.)
        * Sets up default axis values based on first set of x,y values.
        * Will also accept a single sequence of complex values (sets up
            Argand Diagram like reperesentation).
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
    """
    draw = 1
    if 'draw' in keywords.keys():
        draw = keywords['draw']
        del keywords['draw']
    if not args:
        x = Num.ravel(Num.array(x))
        if type(x[0]) == types.ComplexType:
            tplot = plots.CurvePlot(x.real, x.imag, **keywords)
            tplot.axes(squared = 1)
            tplot.curve(symbols = 1, symbolshape='circle')
            tplot.draw()
            return tplot
        raise pxdislin.DislinException('Invalid argument(s) to mlab.plot')
 
    tplot = plots.CurvePlot(Num.array(x), Num.array(args[0]), **keywords)  
    args = args[1:]
    clr = 1
    for i in range(1,len(args),2):
        curve = pxdislin.Curve(args[i-1], args[i], color=plots.colorloop[clr])
        tplot.add(curve)
        clr += 1
    if draw:
        tplot.draw()
    return tplot
    
    


def pseudocolor(M, draw=1):
    """Draws a psuedo-color representation of a matrix.
    
    Comments:
        * Use keyword setting draw=0 to just return the plot object without
            first drawing the plot.
            
        * Routine actually inverts the ordering of the matrix so it is
        viewed as we generally think of matrices (i.e. with row indices
        increasing as we move down, and column indices increasing as we
        move to the right)
    """
    nr, nc = M.shape
    plot = contours.ColorPlot()
    ai = plots.auto_axis(Num.ravel(M))
    plot.axes.zaxis(min = ai.min, max = ai.max, 
                    tickstart = ai.start, tickstep = ai.step)
    plot.axes(autoresolution = (nr,nc))
    clrmatrix = contours.ColorMatrix(MLab.flipud(M), nr, nc)
    plot.add(clrmatrix)
    
    if draw:
        plot.draw()
    return plot
    


#------------------------------------------------------------------------------
# Demo functions

#------------------------------------------------------------------------------
##  $Log: mlabplots.py,v $
##  Revision 1.1.1.1  2002/04/11 15:18:55  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.2  2002/03/09 01:12:37  pmagwene
##  removed disipyldoc
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.3  2001/03/27 03:00:00  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.2  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
