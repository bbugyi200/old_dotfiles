""" Miscellaneous disipyl plots

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

##  $Id: miscplots.py,v 1.1.1.1 2002/04/11 15:18:55 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""


#------------------------------------------------------------------------------

import Numeric as Num
import utilities, pxdislin, pxdislin3D, plots

#------------------------------------------------------------------------------
# 2D Edges and GraphPlots

class Edges(pxdislin.LineGroup):
    """Object representing sets of edges for constructing graph objects.
    
    Input are sequences of x and y coordinates and a sequences
    of edge tuples indicating which vertices share an edge.
    """    
    def __init__(self, xlist, ylist, edges, **keywords):
        pxdislin.LineGroup.__init__(self, **keywords)
        lines = []
        for edge in edges:
            s, e = edge
            startx, starty = xlist[s], ylist[s]
            endx, endy = xlist[e], ylist[e]
            l = pxdislin.Line(startx, starty, endx, endy)
            lines.append(l)
        self.objects = lines
        
    


class GraphPlot(plots.ScatterPlot):
    """Draws a Graph - set of vertices and edges.
    
    Given a set of 2D vertices, and a sequences of edges [tuples which
    indicate the pairs of pts which are joined by an edge], draw
    a graph of the pts and the edges.
    
    """
    def __init__(self, xlist, ylist, edges=[],labels=None, **keywords):
        self.edgedata = edges
        plots.ScatterPlot.__init__(self, xlist, ylist,labels=labels, **keywords)
        
    def set_axes(self):
        plots.ScatterPlot.set_axes(self)
        self.axes(squared = 1)
        
    def set_dataobjects(self):
        plots.ScatterPlot.set_dataobjects(self)
        self.edges = Edges(self.xlist, self.ylist, self.edgedata)
        self.add(self.edges)
        

class PathPlot(GraphPlot):
    """Given a set of vertices and a path through those vertices.
    
    The path is a sequence (list or tuple) specifying the order in which
    vertices are visited.
    """
    def __init__(self, xlist, ylist, path=[], labels=None, **keywords):
        edges = zip(path,path[1:])
        GraphPlot.__init__(self, xlist, ylist, edges, labels, **keywords)
        
#------------------------------------------------------------------------------
# 3D Edges and GraphPlots

class Edges3D(Edges):
    def __init__(self, xlist, ylist, zlist, edges, **keywords):
        pxdislin.LineGroup.__init__(self, **keywords)
        lines = []
        for edge in edges:
            s, e = edge
            startx, starty, startz = xlist[s], ylist[s], zlist[s]
            endx, endy, endz = xlist[e], ylist[e], zlist[e]
#            xl = [startx,endx]
#            yl = [starty,endy]
#            zl = [startz,endz]
            l = pxdislin3D.Line3D(startx,starty,startz,endx,endy,endz)
            lines.append(l)
        self.objects = lines            

class Path3D(Edges3D):
    def __init__(self, xlist, ylist, zlist, path, **keywords):
        Edges3D.__init__(self, xlist, ylist, zlist, zip(path, path[1:]), **keywords)




class GraphPlot3D(plots.ScatterPlot3D):
    """Draws a Graph - set of vertices and edges.
    
    Given a set of 2D vertices, and a sequences of edges [tuples which
    indicate the pairs of pts which are joined by an edge], draw
    a graph of the pts and the edges.
    
    """
    def __init__(self, xlist, ylist, zlist, edges=[], **keywords):
        self.edgedata = edges
        plots.ScatterPlot3D.__init__(self, xlist, ylist,zlist, **keywords)
        
    def set_axes(self):
        plots.ScatterPlot3D.set_axes(self)
        self.axes(squared = 1)
        
    def set_dataobjects(self):
        plots.ScatterPlot3D.set_dataobjects(self)
        self.edges = Edges3D(self.xlist, self.ylist, self.zlist, self.edgedata)
        self.add(self.edges)
        

class PathPlot3D(GraphPlot3D):
    """Given a set of vertices and a path through those vertices.
    
    The path is a sequence (list or tuple) specifying the order in which
    vertices are visited.
    """
    def __init__(self, xlist, ylist, zlist, path=[], **keywords):
        edges = zip(path,path[1:])
        GraphPlot3D.__init__(self, xlist, ylist, zlist, edges,  **keywords)

#------------------------------------------------------------------------------
# Grid and GridPlot


class Grid(pxdislin.DrawObject):
    """Object for representing a deformed 2D grid.
    
    Comments:
        * Input is a Numeric matrix, either a 2D matrix of complex elements,
          or an (m,n,2) matrix of real elements. 
    """
    def __init__(self, matrix=None, **keywords):
        disfunc = None
        pxdislin.DrawObject.__init__(self,disfunc,**keywords)
        if matrix is not None:
            self.matrix = matrix

    def set_default_options(self):
        pxdislin.DrawObject.set_default_options(self)
        self.__options__.extend(['linestyle','interpolation'])
        self.linestyle = 'solid'
        self.interpolation = 'linear'

 
    def draw(self):
        pxdislin.DrawObject.draw(self)
        for line in self.lines:
            line.draw()
        
    def configure(self):
        cmtx = utilities.toComplex(self.matrix)
        self.lines = []
        for row in cmtx:
            xl, yl = row.real, row.imag
            curve = pxdislin.Curve(xl,yl)
            curve(style = self.linestyle, interpolation = self.interpolation)
            self.lines.append(curve)        
        
        for col in Num.transpose(cmtx):
            xl, yl = col.real, col.imag
            curve = pxdislin.Curve(xl,yl)
            curve(style = self.linestyle, interpolation = self.interpolation)
            self.lines.append(curve)


        
class GridPlot(plots.Plot2D):
    """Plot object for displaying deformed 2D coordinates.
    
    Useful for displays of self-organizing maps (SOMs) and
    Thin Plate Splines (TPS).
    """
    def __init__(self, matrix, **keywords):
        self.matrix = matrix
        plots.Plot2D.__init__(self, **keywords)
        
    def set_axes(self):
        plots.Plot2D.set_axes(self)
        cmtx = utilities.toComplex(self.matrix)
        all = Num.ravel(cmtx)
        allx, ally = all.real, all.imag
        ai = plots.auto_axes2D(allx, ally, squared = 1)
        self.axes.xaxis(min = ai.xmin, max = ai.xmax,
                        tickstart = ai.xstart, tickstep = ai.xstep)
        self.axes.yaxis(min = ai.ymin, max = ai.ymax,
                        tickstart = ai.ystart, tickstep = ai.ystep)            
        self.axes(squared = 1)
        
    
    def set_dataobjects(self):
        plots.Plot2D.set_dataobjects(self)
        self.grid = Grid(self.matrix)
        self.add(self.grid)
 

#------------------------------------------------------------------------------
# Phase Diagrams

class PhaseDiagram2D(plots.Plot2D):
    def __init__(self, xfunc, yfunc, **keywords):
        pass


#------------------------------------------------------------------------------
##  $Log: miscplots.py,v $
##  Revision 1.1.1.1  2002/04/11 15:18:55  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.2  2002/01/29 02:51:32  pmagwene
##  changed to __options__ mechanism
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.4  2001/06/05 02:48:43  pmagwene
##  *** empty log message ***
##
##  Revision 1.3  2001/04/24 13:53:36  pmagwene
##  added 3D Edges and GraphPlot
##
##  Revision 1.2  2001/04/20 02:53:53  pmagwene
##  minor tweaks
##
##  Revision 1.1  2001/04/05 14:44:20  pmagwene
##  Messed around with __setattr__ in base Object
##  Added miscplots.py - for useful plots that don't deserver to be
##   in plots.py (which should be rserved for the most basic plots)
##    
