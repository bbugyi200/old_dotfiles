"""Demos illustrating the disipyl library.

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
##  $Id: demos.py,v 1.4 2010/06/26 21:49:58 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@duke.edu>"
__version__ = "$Revision: 1.4 $"
__credits__ = ""

#------------------------------------------------------------------------------
import math
hasnumeric = 1
try:
    import numpy as Num
except ImportError:
    hasnumeric = 0
    import utilities
    Num = utilities.Num


#------------------------------------------------------------------------------

import pydislin, pxdislin, pxdislin3D, plots, contours, utilities

#------------------------------------------------------------------------------

def curveDemo(ret=None):
    """A fairly involved demo, building a plot from the basic components.
    
    Comments: Normally I'd probably start with a plots.FunctionPlot object to
        draw one of the function curves, and then add the second curve.  However,
        this demo shows how you can build up the plot from simple objects.
    * This demo also shows that you don't necessarily need a Canvas object to 
        draw a plot.  If needed a PlotObject will instantiate it's own Canvas.
    * Legend object is used.  Note that by default, Legend objects will automatically
        take their properties from calls to dislin.curve (usually via Curve object).
        If you want to change this behavior setting the auto property of the legend
        object to zero, and define symbols, linestyles, etc. for the legend object.
    * Note that the title uses TeX mode.
    """
    
    title = "Demonstration of Curve objects\n"+\
            "$\cos x, \sin x$"

    # generate data
    x = range(0, 360, 2)
    f1 = [math.cos((i/180.)*math.pi ) for i in x]
    f2 = [math.sin((i/180.)*math.pi ) for i in x]    

    # setup plot    
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title)

    # setup axes
    plot.axes(pagesize = (2200,1200),
           originlines = "y")   # draws line at origin of y-axis    
    plot.axes.xaxis(min = 0,
                 max = 360,
                 tickstart = 0,
                 tickstep = 90,
                 ticknumber = 10,
                 name = 'X-Axis')    
    plot.axes.yaxis(min = -1,
                 max = 1,
                 tickstart = -1,
                 tickstep = 0.5,
                 ticknumber = 10,
                 name = 'Y-Axis')
    
    # add Curve objects
    curve1 = pxdislin.Curve(x, f1, color = 'green')
    curve2 = pxdislin.Curve(x, f2, color = 'red')
    plot.add(curve1, curve2) 
    
    # add Legend
    leg = pxdislin.Legend(textlines = ['$\cos x$', '$\sin x$'], position='axisUR')
    plot.add(leg)
    
        
    # I'm doing this so I can return the demo objects as well as just draw 'em
    # This is convenient for debugging and for passing objects to the GUI demos
    # Note that you can tell the Plot object to draw itself rather
    # than explicitly passing it to a canvas object    
    if ret:
        canvas = pxdislin.Canvas(plot)
        return canvas
    else:
        plot.draw()


#------------------------------------------------------------------------------
    
def contourDemo1(ret=None):
    """Demonstrates use of FunctionContours object. 
    
    Comments:  FunctionContours make plotting contours of particular functions
        easy, by taking care of all the calculations for you.  
    """
    
    title = "Demonstration of FunctionContours\n\n" +\
            "$f(x,y)=cos(x)sin(y)$"

    def f(x,y):
        return math.cos(x*math.pi/180) * math.sin(y*math.pi/180)
        
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title)
    
    plot.axes.limits(0, 360, 0, 360)
    plot.axes.ticks(0, 90, 0, 90)
    plot.axes.xaxis(name='X-Axis')
    plot.axes.yaxis(name='Y-Axis')
        
    cntrs = contours.FunctionContours(f, 0, 360, 10, 0, 360, 10, zstep=0.2, 
                                      colored=1, colorstart=30, colorstep=30,
                                      labeltype='float') 
    plot.add(cntrs)
           
    canvas = pxdislin.Canvas(plot)
    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------    
    
def contourDemo2(ret=None):
    """Demonstrates use of Contour objects, ShadedContours, and the Canvas2UP object.
    
    Comments:   
    * Use of individual Contour ojects rather than a FunctionContours 
       object allows for a greater amount of tweaking.  
    * I use a Loop object (from disipyl.utilies) for cycling attributes.
    * I generate two contour plots of the same function, one using standard
       Contours, and one using a ShadedContours object.
    """
    
    title = "Demonstration of Contours\n\n" +\
            "$f(x,y) = (x^2-1)^2 + (y^2-1)^2$"

    def f(x,y):
        return (x**2 - 1)**2 + (y**2 - 1)**2
        
    # generate data
    xlist = Num.arange(0, 1.6, 0.03)
    ylist = Num.arange(0, 1.6, 0.03)
    zlist = utilities.zcalculator(f, xlist, ylist)
    levels = Num.arange(1.3, 0, -0.1)
    
    # setup the first plot    
    plot1 = plots.Plot2D()
    plot1.title = pxdislin.Title(text = title)
    
    plot1.axes(pagesize = (1200,2200))
    plot1.axes.xaxis(min = 0,
                     max = 1.6, 
                     tickstart = 0, 
                     tickstep = 0.2, 
                     name = 'X-Axis')
    plot1.axes.yaxis(min = 0, 
                     max = 1.6,
                     tickstart = 0, 
                     tickstep = 0.2, 
                     name = 'Y-Axis')
    
    # generate contours, with repeating line types (see disipyl.utilties)
    stylecycle = utilities.Loop(['dot','dash','solid'])
    widthcycle = utilities.Loop([1,1,3])
    cntrs = []
    for i in range(12):
        cntr = contours.Contour(xlist, ylist, zlist, levels[i])
        cntr(linestyle = stylecycle[i], linewidth = widthcycle[i])
        cntrs.append(cntr)
                                                
    plot1.add(*cntrs)       # note how I add a sequence of contours, a single contour
                            # would be added as plot.add(contour)
                            # you could also do this as [plot1.add(c) for c in contours]

    
    # setup the second plot                    
    plot2 = plots.Plot2D()
    plot2.title = pxdislin.Title(text = title)
    
    plot2.axes(pagesize = (1200,2200))
    plot2.axes.xaxis(min = 0, 
                     max = 1.6, 
                     tickstart = 0, 
                     tickstep = 0.2, 
                     name = 'X-Axis')
    plot2.axes.yaxis(min = 0,
                     max = 1.6, 
                     tickstart = 0, 
                     tickstep = 0.2, 
                     name = 'Y-Axis')
    
    shdcntrs = contours.ShadedContours(xlist, ylist, zlist, levels)
    shdcntrs(fillmode = 'polygon')
    plot2.add(shdcntrs)
         
    canvas = pxdislin.Canvas2UP(plot1,plot2)    # Note use of Canvas2UP
    canvas.page(width = 4500, height = 3500)    # change default page height

    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------
        
def colorPlot3DDemo(ret=None):
    """Demonstrates 3-D color plot.
    
    Comments:
    """
    if not hasnumeric:
        print "demos.colorPlot3DDemo requires numpy"
        return None

    title = "Demonstration of 3-D Color Plot \n\n" +\
            "$f(x,y)=cos(x)sin(y)$"

    def f(x,y):
        return math.cos(x*math.pi/180) * math.sin(y*math.pi/180)
        
    # generate data
    x = range(0, 360, 3)
    y = range(0, 360, 3)
    zlist = utilities.zcalculator(f,x,y)
    zmatrix = Num.reshape(zlist, (len(x), len(y)))

    # setup plot        
    plot = contours.ColorPlot()
    plot.title = pxdislin.Title(text = title)
    
    plot.axes(lengths = (2200,1400,1400),
              pageposition = (300,1850),
              autoresolution = (len(x), len(y)) )
    plot.axes.xaxis(min = 0, 
                    max = 360, 
                    tickstart = 0, 
                    tickstep = 90, 
                    name = 'X-Axis')
    plot.axes.yaxis(min = 0, 
                    max = 360, 
                    tickstart = 0, 
                    tickstep = 90, 
                    name = 'Y-Axis')
    plot.axes.zaxis(min = -1, 
                    max = 1, 
                    tickstart = -1, 
                    tickstep = 0.5, 
                    name = 'Z-Axis')
    
    # Create 3-D color representation of a matrix or list
    nr, nc = zmatrix.shape
    colormatrix = contours.ColorMatrix(zmatrix, nr, nc)
    plot.add(colormatrix)
        
    canvas = pxdislin.Canvas(plot)
    if ret:
        return canvas
    else:
        canvas.draw()
    
#------------------------------------------------------------------------------

def fsurf(x,y):
    """function used in functionSurfaceDemo"""
    return math.cos(x*math.pi/180) * math.sin(y*math.pi/180)
    
def functionSurfaceDemo(ret=None):
    """Demonstrates SurfacePlot object (wrapper around FunctionSurface).
    
    Comments:
    """
    if pydislin.withjython:
        print """plots.SurfacePlot does not work correctly with Jython"""

    title = "Demonstration of 3-D SurfacePlot \n\n" +\
            "$f(x,y)=cos(x)sin(y)$"

    plot = plots.SurfacePlot(fsurf, 0, 360, 10, 0, 360, 10)
    plot.surface(topcolor = 55, bottomcolor = 220)
    plot.title = pxdislin.Title(text = title)
    
    plot.axes(border = 1,                   # draw a 3D box around axes
              pagesize = (2200,2200), 
              centered=1)
    plot.axes.xaxis(min = 0, 
                    max = 360, 
                    tickstart = 0, 
                    tickstep = 90, 
                    name = 'X-Axis')
    plot.axes.yaxis(min = 0, 
                    max = 360, 
                    tickstart = 0, 
                    tickstep = 90, 
                    name = 'Y-Axis')    
    plot.axes.zaxis(min = -1.5, 
                    max = 1.5, 
                    tickstart = -1.5, 
                    tickstep = 0.5, 
                    name='Z-Axis')

    
    canvas = pxdislin.Canvas(plot)
    canvas.page(width = 3000, height = 3000)
    
    if ret:
        return canvas
    else:
        canvas.draw()
        
#------------------------------------------------------------------------------

def functionSurfacePlusPlane(ret=None):
    """Demonstrates SurfacePlot plus Plane3D objects.
    
    Comments:
        * I combine a 2D representation and a 3D representation of the same
            function into a single 3D plot.
    """            
    
    title = "Demonstration of 3-D SurfacePlot plus Plane3D \n\n" +\
            "$f(x,y)=cos(x)sin(y)$"
    
    plot1 = functionSurfaceDemo(1).plot    # Note I'm getting the plot from the canvas object
    plot1.axes(border = 0)
    plot1.title = pxdislin.Title(text=title, offset=-200)
    plot1.rotate_left(5)
    
    plot2 = contourDemo1(1).plot
    plot2.title = None
    plot2.axes.ticks(90, 90, 90, 90)
    
    plane = pxdislin3D.Plane3D(plot2)
    plot1.add(plane)
    
    canvas = pxdislin.Canvas(plot1)
    canvas.page(width = 3000, height = 3000)
    
    if ret:
        return canvas
    else:
        canvas.draw()
    
#------------------------------------------------------------------------------

def AllTrigDemo(ret=None):
    """Combines a bunch of plots onto a Canvas4UP object."""
    
    plot1 = contourDemo1(1).plot
    plot2 = colorPlot3DDemo(1).plot
    plot2.axes(lengths = (2500,2500,2500))
    plot3 = functionSurfaceDemo(1).plot
    
    msg1 = "This complicated graph was"
    msg2 = "constructed using the"
    msg3 = "Canvas4UP Object"
    text1 = pxdislin.Text(text=msg1, position=(4000,4000), height=100, ucoords=0)
    text2 = pxdislin.Text(text=msg2, position=(4000,4200), height=100, ucoords=0)
    text3 = pxdislin.Text(text=msg3, position=(4000,4400), height=100, ucoords=0)
    
    canvas = pxdislin.Canvas4UP(plot1, plot2, plot3)
    canvas.add(text1, text2, text3)
    canvas.page(width = 7200, height = 7200)
       
    if ret:
        return canvas
    else:
        canvas.draw() 
#----------------------------------------------------------------------------

def CanvasMultiPlotDemo(ret=None):
    """Demonstrates how to use the CanvasMultiPlot object.
    
    """

    plot1 = contourDemo1(1).plot
    plot2 = colorPlot3DDemo(1).plot
    plot2.axes(lengths = (2500,2500,2500))
    plot3 = functionSurfaceDemo(1).plot
    
    plot1.axes(pageposition = (300,3000),
                 pagesize     = (2500,2500),
                 axisparts    = ('all','all','none','none')
                 )
    plot2.axes(pageposition = (3100,3000),
                  pagesize     = (2500,2500),
                  axisparts    = ('all','none','none','none')
                  )

    
    canvas = pxdislin.CanvasMultiPlot([plot1, plot2])
    canvas.page(width = 6000, height = 3500)
    canvas(fileoverwrite=1)
    canvas.draw()
        


#------------------------------------------------------------------------------

def pfunc(x, y, axis):
    """function for parametricSurfaceDemo."""
    if axis == 1:
        xv = math.cos(x)*(3+math.cos(y))
    elif axis == 2:
        xv = math.sin(x)*(3+math.cos(y))
    else:
        xv = math.sin(y)
    return xv


def parametricSurfaceDemo(ret=None):
    """Demonstrates ParametricSurface object.
    
    Comments:  Plot is drawn with black background by setting the screenmode
     attribute of the canvas object to 'black' (as opposed to 'normal').
    """    
    if pydislin.withjython:
        print """pxdislin3D.ParametricSurface does not work correctly with Jython"""

    title = "Demonstration of 3-D ParametricSurface Plot \n\n" +\
            "$f(u_1,u_2)= \\cos u_1 \\times (3+ \\cos u_2),\\;" +\
            "\\sin u_1 \\times (3+ \\cos u_2),\\; \\sin u_2$"    
    
    plot = plots.Plot3D()
    plot.title = pxdislin.Title(text = title)
    
    plot.axes(pagesize=(2200,2200), pageposition=(400,2800))
    plot.axes.xaxis(min=-4, 
                    max=4, 
                    tickstart = -4, 
                    tickstep=1)
    plot.axes.yaxis(min=-4, 
                    max=4,
                    tickstart = -4,
                    tickstep=1)
    plot.axes.zaxis(min=-3,
                    max=3,
                    tickstart = -3,
                    tickstep=1)
    
    step = (2*math.pi)/30.
    
    surface = pxdislin3D.ParametricSurface(pfunc, (0,2*math.pi,step), (0,2*math.pi,step))
    surface(colorscale=(-1,1), shadingmode='smooth')
    plot.add(surface)

    canvas = pxdislin.Canvas(plot)
    canvas(screenmode = 'black')
    canvas.page(width = 3000, height=3000) 

    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------

def surfaceDemo(ret=None):
    """Shows of the IrregularSurface and ShadedSurface Plots."""
    
    title1 = "Plot created with IrregularSurface Object \n\n"+\
             " $f(x,y) = x^2 - y^2$"

    title2 = "Plot created with ShadedSurface Object \n\n"+\
             " $f(x,y) = x^2 - y^2$"    
    
    # setup data
    def f(x,y): return x**2 - y**2
    xlist = Num.arange(-1,1,0.1)
    ylist = Num.arange(-1,1,0.1)
    zlist = utilities.zcalculator(f, xlist, ylist)
    
    # setup plots         
    plot1 = plots.Plot3D()
    plot1.title = pxdislin.Title(text = title1, offset = -400)
    plot1.axes(pagesize = (2000,2000))    
    irsurf = pxdislin3D.IrregularSurface(xlist, ylist, zlist)
    plot1.add(irsurf)
    
    plot2 = plots.Plot3D()
    plot2.title = pxdislin.Title(text = title2, offset = -200)
    plot2.axes(pagesize = (2000,2000), centered = 1)
    shdsurf = pxdislin3D.ShadedSurface(xlist, ylist, zlist)
    plot2.add(shdsurf)
    # we're gonna draw this one at a different angle
    plot2.rotate_left(12)
    plot2.rotate_down(10)  
        
    canvas = pxdislin.Canvas(plot2, screenmode='black')
    canvas.page(width = 3000, height = 3000)    

    # I need to figure out why DISLIN doesn't like a 2UP Canvas with
    # an Irregular surface object (calling draw causes crash)    
    canvas2 = pxdislin.Canvas2UP(plot1, plot2)
    canvas2.page(width = 6000, height = 3000)
    
    if ret:
        return canvas
    else:
        canvas.draw()
   

#------------------------------------------------------------------------------
                
def barDemo(ret=None):
    """Demonstrates Bars object."""
    
    title = "Bar Graph"
    
    # setup data
    x = range(1,10)
    y0 = [0]*10
    y1 = [1,2,3,4,5,4,3,2,1]
    
    # setup  plot
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title)
    
    plot.axes(integerlabels=1) 
    plot.axes.limits(0,10, 0,6)
    plot.axes.ticks(1, 1, 1, 1)
       
    bars = pxdislin.Bars(x, y0, y1)
    bars(filled = 1)
    plot.add(bars)
    
    canvas = pxdislin.Canvas(plot)
    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------
def labeledBarDemo(ret=None):
    """Demonstrates Bars object."""
    
    title = "Bar Graph"
    
    # setup data
    xlabels = ['Chicago', 'New York', 'San Fran', 'L.A.', 'Boston', 'Atlanta',
               'Dallas', 'Seattle', 'St. Louis' ]

    x = range(1, len(xlabels) + 1)
    y0 = [0]*len(x)
    y1 = [1,2,3,4,5,4,3,2,1]
    
    # setup  plot
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title, offset=-200)
    
    plot.axes(integerlabels=1, centered = 1) 
    plot.axes.limits(0,10, 0,6)
    plot.axes.ticks(1, 1, 1, 1)
    plot.axes.xaxis(labeltype = 'mylab', mylabels = xlabels,
                    labelorient = 'vertical')
    bars = pxdislin.Bars(x, y0, y1, filled = 1)
    plot.add(bars)
    
    canvas = pxdislin.Canvas(plot)

    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------

def multiBarDemo(ret=None):
    """Demonstrates use of multiple Bars objects."""
    
    title = "(Multi)Bar Graph"
    
    # setup data
    x = range(1,10)
    y0 = [0]*10
    y1a = [1,2,3,4,5,4,3,2,1]
    y1b = [2,1,4,3,6,4.5,2,3,1]
    
    # setup  plot
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title)
    
    plot.axes(integerlabels=1) 
    plot.axes.limits(0, 10, 0, 7)
    plot.axes.ticks(1, 1, 1, 1)
         
    barsA = pxdislin.Bars(x, y0, y1a, filled = 1)
    barsA(labelmode = 'second') # draw labels above bars using endpoints as values
    # Set options for drawing multiple bar groups.  Only have to do this 
    # for the first Bars object
    barsA(groupbars = 1, ngroups = 2, groupgap = 0)
       
    barsB = pxdislin.Bars(x, y0, y1b, filled = 1, pattern = 'hatch1', color=230)
    barsB(labelmode = 'second') # draw labels above bars using endpoints as values

    plot.add(barsA,barsB)
    
    canvas = pxdislin.Canvas(plot)
    if ret:
        return canvas
    else:
        canvas.draw()
        
#------------------------------------------------------------------------------
# AllBars

def AllBarsDemo(ret=None):
    """Combines bar demos into single canvas."""
    
    plot1 = barDemo(1).plot
    plot1.title(offset = -200)
    
    plot2 = multiBarDemo(1).plot
    plot2.title(offset = -200)
    
    canvas = pxdislin.Canvas2UP(plot1,plot2)
    canvas.page(width = 5000, height = 3500)
    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------

def scatterDemo(ret=None):
    """Example of ScatterPlot usage."""
    
    title = "Scatter Plot"
    
    # data
    x = [61, 37, 65, 69, 54, 93, 87, 89, 100, 90, 97]
    y = [14, 17, 24, 25, 27, 33, 34, 37, 40, 41, 42]
    
    plot = plots.ScatterPlot(x,y, symboloptions={'shape':'fill circle','size':40})
    plot.title = pxdislin.Title(text = title)
    
    canvas = pxdislin.Canvas(plot)
    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------

def multiScatterDemo(ret=None):
    """Example of using symbol groups to construct a scatter plot.
    
    Comments:
    -- Also demonstrates use of Legend object.
    """
    
    title = "Multi-Scatter Plot"
    
    # data
    xlist = [[3,4,5],[6,10,12],[8,9,11]]
    ylist = [[0,1,3],[2,5,6],[7,11,12]]
    
    # plot
    symbols = ['fill triangle', 'fill circle', 'fill square']
    colors = [50,150,230]
    
    plot = plots.Plot2D()
    plot.title = pxdislin.Title(text = title)

    # This is a quicker way to set axis limits and ticksteps
    plot.axes.limits(-1, 15, -1, 15)
    plot.axes.ticks(0, 5, 0, 5)
 
    
    groups = []
    for i in range(len(xlist)):
        groups.append(pxdislin.SymbolGroup(xlist[i], ylist[i],
                      shape=symbols[i], color=colors[i], size=40))
    plot.add(*groups)
    
    canvas = pxdislin.Canvas(plot)
    canvas.page(width = 3000, height = 2200)
    
    # setup legend
    ltext = ['group 1', 'group 2', 'group 3']
    legend = pxdislin.Legend(textlines=ltext, 
                             auto=0,
                             position=(2200, 400),
                             colors = colors, 
                             symbols = symbols)        
    
    canvas.add(legend)  # You can add the legend to either the canvas object or the
                        # plot object with the same effect
    
    if ret:
        return canvas
    else:
        canvas.draw()    

#------------------------------------------------------------------------------
# AllScatter

def AllScattersDemo(ret=None):
    """Combines scatter demo's into a single larger canvas."""
        
    plot1 = scatterDemo(1).plot
    plot1.title(offset = -200)
    
    plot2 = multiScatterDemo(1).plot
    plot2.title(offset = -200)
    
    canvas = pxdislin.Canvas2UP(plot1,plot2)
    canvas.page(width = 5000, height = 3500)
    if ret:
        return canvas
    else:
        canvas.draw()
        
        
#------------------------------------------------------------------------------
# ColorDemo

def colorDemo(ret=None,colortable='rainbow'):
    """Generates a color bar showing values of color indices."""
    if not hasnumeric:
        print "demos.colorDemo requires Numeric"
        return None
    
    def f(x,y):
        return x
        
    # data
    x = range(0, 256)
    y = range(0, 2)
    zlist = utilities.zcalculator(f,x,y)
    zmatrix = Num.reshape(zlist, (len(x), len(y)))
    
    # plot
    plot = contours.ColorPlot()

    plot.axes(autoresolution = (len(x), len(y)),
              axisparts=('all','none','none','none'), 
              suppresscolorbar=1,
              lengths = (2000,500,500),
              pageposition = (150,650),
              integerlabels = 1)    
    plot.axes.xaxis(min = 0, 
                    max = 255, 
                    tickstart = 0,
                    tickstep = 50, 
                    name = 'Color Index')
    plot.axes.yaxis(min = 0,
                    max = 1, 
                    tickstart = 0, 
                    tickstep = 0.5)
    plot.axes.zaxis(min = 0, 
                    max = 255, 
                    tickstart = 0, 
                    tickstep = 25)


    
    # Create 3-D color representation of a matrix or list
    nr, nc = zmatrix.shape
    colormatrix = contours.ColorMatrix(zmatrix, nr, nc)
    plot.add(colormatrix)
       
    canvas = pxdislin.Canvas(plot, colortable=colortable)
    canvas.page(width = 2400, height = 1000, scalingmode='full')

    if ret:
        return canvas
    else:
        canvas.draw()

#------------------------------------------------------------------------------
# Lighting Demo

def lightingDemo(ret=None):
    """This is meant to demonstrate the using of LightingSystem objects.  NOT WORKING YET!"""
    
    plot = plots.Plot3D()
    sphere = pxdislin3D.Sphere(0,0,0, 0.5, color = 40)
    plot.add(sphere)
    
    lights = pxdislin3D.LightingSystem()
    lt1 = pxdislin3D.Light(coordinatesystem='user',
                           position=(0.9,-0.9,0.9),
                           lighttype=('specular',1.5),
                           material=('specular',0.5))
    lights.addLight(lt1, 1)
    
    canvas = pxdislin.Canvas(p, suppressmessages = 0)
    canvas.add(lights)
    
    canvas.draw()

#------------------------------------------------------------------------------

# useful for passing to other modules for exercising demos
demolist = [curveDemo, contourDemo2, contourDemo1, colorPlot3DDemo,
    functionSurfaceDemo, functionSurfacePlusPlane, parametricSurfaceDemo,
    surfaceDemo, labeledBarDemo, AllBarsDemo, AllScattersDemo]


def showdemos():
    """calls each of the demos in turn."""
    curveDemo()
    contourDemo2()
    contourDemo1()
    colorPlot3DDemo()
    functionSurfaceDemo()
    functionSurfacePlusPlane()
    parametricSurfaceDemo()
    surfaceDemo()
    labeledBarDemo()
    AllBarsDemo()
    AllScattersDemo()
    CanvasMultiPlotDemo()
    
if __name__ == '__main__':
    showdemos()



#------------------------------------------------------------------------------
##  $Log: demos.py,v $
##  Revision 1.3  2002/04/17 21:49:58  pmagwene
##  added Reggie Dugard's mylab demo example
##  small fix to pydislin.py
##
##  Revision 1.2  2002/04/11 16:19:42  pmagwene
##  Added wxdisipyl.py
##
##  Revision 1.1.1.1  2002/04/11 15:18:54  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.2  2002/03/09 01:12:37  pmagwene
##  removed disipyldoc
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.13  2001/04/17 20:05:09  pmagwene
##  *** empty log message ***
##
##  Revision 1.12  2001/03/28 14:42:13  pmagwene
##  Cleanup and bug-fixes.
##
##  Revision 1.11  2001/03/27 03:00:00  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems 
##
##  Revision 1.10  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
##  Revision 1.9  2001/03/25 20:19:56  pmagwene
##  Modified demos to return Canvas rather than plot objects
##  Renamed tkdislin -> tkdisipyl
##  Fleshed out tkdisipyl
##  Added cool 2D and 3D demos to tkdisipyl
##
##  Revision 1.8  2001/03/23 19:08:30  pmagwene
##  *** empty log message ***
##
##  Revision 1.7  2001/03/23 00:22:51  pmagwene
##  Some tweaknig of AxisSystem2D, Axis, and related functions to give better defaults.
##  Started mlabplot -- a set of functions with similar functionality to their counterparts in MATLAB.
##
##  Revision 1.6  2001/03/20 03:24:44  pmagwene
##  Added module for automatic documentation (disipyldoc.py)
##  Added doc strings to most objects.
##
##  Revision 1.5  2001/03/19 22:58:06  pmagwene
##  Tinkering with demos, minor modifications of pxdislin and utilities
##
##  Revision 1.4  2001/03/19 08:53:00  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.3  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##
