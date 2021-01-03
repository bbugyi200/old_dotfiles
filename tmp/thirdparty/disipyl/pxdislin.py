"""Base classes for creating disipyl plots.

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
##  $Id: pxdislin.py,v 1.8 2002/04/23 18:18:30 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.8 $"
__credits__ = ""

#------------------------------------------------------------------------------

import string, types, math, copy, os.path, cPickle, sys, string

#------------------------------------------------------------------------------
try:
    import Dislin       # try the java class library first
    dislin = Dislin 
except:
    import dislin
#------------------------------------------------------------------------------
import pydislin as pdis

import utilities
from utilities import allExist, pickleObject, unpickleObject


#------------------------------------------------------------------------------
class DislinException(Exception):
    """Exception type for disipyl errors."""
    pass


def my_import(name):
    mod = __import__(name)
    components = string.split(name, '.')
    for comp in components[1:]:
        mod = getattr(mod, comp)
    return mod



def info(instance, option):
    if option not in instance.__dict__.keys():
        print "No such option for class: %s" % instance.__class__.__name__
        return None
    if hasattr(instance, "infomodule"):
        try:
            infomodule = my_import(instance.infomodule)
        except ImportError:
            infomodule = None
        if infomodule is not None:
            cls = instance.infoclass
            try:
                print getattr(infomodule.__dict__[cls], option)
                return
            except KeyError:
                pass
    print "Undocumented option."


#----------------------------------------------------------------------------
# Version juggling

if sys.version_info[0] < 2:
    raise DislinException("This version of disipyl requires Python 2.0 or later")

if sys.version_info[1] < 2:
    # class "object" and __getattribute__ were introduced in Python 2.2
    class object:
        def __getattribute__(self, name):
            return self.__dict__[name]

        def __setattr__(self, name, value):
            self.__dict__[name] = value


#------------------------------------------------------------------------------
# Object Base Class

class Object(object):
    """Base class for all disipyl drawing objects."""
    def __init__(self, **keywords):
        self.__options__ = []
        self.__data__ = []
        self.set_defaults()
        self.set_keywords(keywords)
        # set data below
            
    def __call__(self, **kw):
        """Set options or data for object."""
        if len(kw) == 0:
            self.print_options()            
        for key in kw.keys():
            if key not in self.__options__ and key not in self.__data__:
                raise KeyError("No such option or data: %s" % key)
            self.__setattr__(key, kw[key])

    def set_keywords(self, kw):
        for key in kw.keys():
            if key not in self.__options__ and key not in self.__data__:
                raise KeyError("No such option or data: %s" % key)
            self.__setattr__(key, kw[key])
                        
            
    def setoptions(self, **kw):
        if not hasattr(self, "__options__"):
            self.__options__ = []
        for key in kw.keys():
            if key not in self.__options__:
                self.__options__.append(key)
            self.__setattr__(key, kw[key])
        if "INFOMODULE" in kw.keys():
            self.set_info(kw['INFOMODULE'])

    def setdata(self, **kw):
        if not hasattr(self, '__data__'):
            self.__data__ = []
        for key in kw.keys():
            if key not in self.__data__:
                self.__data__.append(key)
            self.__setattr__(key, kw[key])

    def deloption(self, opt):
        self.__options__.remove(opt)
        self.__delattr__(opt)

    def deldata(self, dat):
        self.__data__.remove(dat)
        self.__delattr__(dat)


    def set_defaults(self):
        pass    

    def info(self, option):
        return info(self, option)

    def set_info(self, infomodule):
        module = infomodule[0]
        self.infoclass = infomodule[1]
        self.infomodule = module
                                       
    def option_string(self):
        """Returns string containing all the options and their values."""
        try:
            optionnames = list(self.__options__)
        except AttributeError:
            return ''
        optionnames.sort()
        pstr = ''
        for name in optionnames:
            vstr = str(self.__getattribute__(name))
            if len(vstr) > 80:
                vstr = vstr[:74] + " ..."
            vstr = "%s = %s" % (name, vstr)
            pstr = "%s  %s\n" % (pstr, vstr)
        return pstr 
                
    def print_options(self):
        nstr = self.__class__.__name__
        pstr = '\n*BEGIN OBJECT: %s\n' % nstr
        pstr += self.option_string()
        pstr = "%s*END OBJECT: %s\n" % (pstr, nstr)
        print pstr     
        

    def configure(self):
        pass
             

#------------------------------------------------------------------------------
# Canvas objects

class Canvas(Object):
    """Represents the canvas for drawing disipyl plots.
    
    Comments:
        * Two sequences (dispre, dispost) are provided to enable arbitrary
            dislin/pydislin calls which happen before plots & objects are 
            are drawn (dispre) and after plots & objects are drawn (dispost).
            Elements of these sequences should be added as tuples of form (func, (args)).      
    """
    def __init__(self, plot=None, **keywords):
        Object.__init__(self,**keywords)
        self.page = Page()
        self.plot = plot
        self.objects = []
        self.dispre = []
        self.dispost = []       

    def add(self, *obj):
        self.objects.extend(obj)
        
    def initialize(self):
        pdis.initialize()
        
    def terminate(self):
        pdis.terminate()
        
    def cleanup(self):
        pass
        
    def set_defaults(self): 
        self.setoptions(
            format = 'screen',
            filename = 'dis.out',
            fileoverwrite = 0,
            errorfile = 'dis.err',
            errordevice = 'screen',
            windowapp = 0,
            windowgeom = (None,None,None,None),
            windowsize = (None,None),
            windowkey = 'return',
            windowstore = 1,
            windowID = 0,
            windowtype = 'none',
            windowhold = 'hold',
            colormode = 'full',
            colortable = 'rainbow',
            screenmode = 'normal',
            suppressmessages =  1,
            charactercode = "STANDARD",
            INFOMODULE = ("disipyl.optioninfo.canvas", "Canvas"),
        )

        
    def configure(self):
        pdis.setScreenMode(self.screenmode)   
        pdis.setFormat(self.format)
        pdis.setFilename(self.filename,self.fileoverwrite)
        pdis.setErrorFile(self.errorfile)
        pdis.setErrorDevice(self.errordevice)     
        pdis.setColorMode(self.colormode)
        pdis.setWindowStore(self.windowstore)
        pdis.setWindowID(self.windowID, self.windowtype)
        if self.windowapp:
            pdis.setWindowApp()   

        if allExist(*self.windowgeom):
            pdis.setWindowGeometry(self.windowgeom[0],self.windowgeom[1],\
                                self.windowgeom[2],self.windowgeom[3])
        if allExist(*self.windowsize):
            pdis.setWindowSize(self.windowsize[0], self.windowsize[1])
            
    def configure_level_one(self):
        pdis.setColor('fore')
        if self.suppressmessages:
            pdis.suppressMessages()
        pdis.setWindowKey(self.windowkey)     
        pdis.setWindowHold(self.windowhold)
        if type(self.colortable) == types.StringType:
            pdis.setColorTable(self.colortable)
        if type(self.colortable) == types.TupleType or type(self.colortable) == types.ListType:
            pdis.setMyColorTable(*self.colortable)
        pdis.setCharacterCode(self.charactercode)
        pdis.setHardwareFont()

    def draw(self):
        self.configure()
        self.drawpage()
        try:
            pdis.initialize()
            pdis.clearScreen()
            self.configure_level_one()
            self.drawdispre()
            self.drawplot()            
            self.drawobjects()
            self.drawdispost()
        finally:
            self.cleanup()
            self.terminate()
            
    def drawpage(self):
        if not self.page:
            raise DislinException("No page specified")
        self.page.configure() 

    def drawplot(self):
        if not self.plot:
            raise DislinException("No plot specified")
        self.plot.draw(self)

    def drawobjects(self):
        for obj in self.objects:
            obj.draw()

    def drawdispre(self):
        """Calls func,arg pairs in self.dispre. Happens before plot is drawn."""
        for func, arg in self.dispre:
            func(*arg)        
        
    def drawdispost(self):
        """Calls func,arg pair is self.dispost. Happens after plot and objects are drawn."""
        for func, arg in self.dispost:
            func(*arg)  
                                 
    def save(self, filename, format = 'postscript'):
        """Save plot image to graphic file. 
        """
        oldformat = self.format
        oldname = self.filename
        self(format = format)
        self(filename = os.path.expanduser(filename))
        self.draw()
        self(format = oldformat)
        self(filename = oldname)

    def asbuffer(self, format='png'):
        """Returns a string buffer representation of the DISLIN plot as PNG or PDF.

        Valid formats: 'png' or 'pdf'
        """
        ispdf = 0
        if str(format).lower() == 'pdf':
            buffunc = pdis.getPDFBuffer
            dislin.pdfmod('ON','BUFFER')
            ispdf = 1
        else:
            buffunc = pdis.getPNGBuffer
        
        oldformat = self.format
        self(format = 'virtual')
        if ispdf:
            self(format='pdf')

        self.configure()
        self.drawpage()
        try:
            pdis.initialize()
            pdis.clearScreen()
            self.configure_level_one()
            self.drawdispre()
            self.drawplot()            
            self.drawobjects()
            self.drawdispost()
        finally:
            self.cleanup()
            if not ispdf:
                buf = buffunc()
            self.terminate()
            if ispdf:
                buf = buffunc()
                dislin.pdfmod('OFF','BUFFER')

        self(format = oldformat)
        return buf



class Canvas2UP(Canvas):
    """Canvas object for creating 2 Plots per page.
    
    Comments:
        * Do NOT use the 'centered' option with axes when using Canvas2UP
           or Canvas4UP
    """
    def __init__(self, plotA=None, plotB=None, **keywords):
        Object.__init__(self,**keywords)
        self.page = Page()
        self.plotA, self.plotB = plotA, plotB
        self.objects = []
        self.dispre = []
        self.dispost = []
        self.setup_page()
        
    def set_defaults(self):
        Canvas.set_defaults(self)
        self.setoptions(
            xoffset = 0.08,
            yoffset = 0.08,
            xfraction = 0.80,
            yfraction = 0.80,  
            nvert = 1,
            nhoriz = 2, 
            INFOMODULE = ("disipyl.optioninfo.canvas", "Canvas2UP"),
        )    
        
    def setup_page(self):
        self.page(width = 5000, height = 2500)  
        
    def drawplot(self):
        if not self.plotA:
            raise DislinException("Must specify at least one plot for Canvas2UP")
        self.drawplotA()
        if self.plotB:
            self.drawplotB()
            
    def drawplotA(self):
        oposition = self.plotA.axes.pageposition
        osize = self.plotA.axes.pagesize
        ocenter = self.plotA.axes.centered
        width = self.page.width
        height = self.page.height
        x = width * self.xoffset
        y = (height/self.nvert) - ((height/self.nvert) * self.yoffset) 
        size = ((width/self.nhoriz) * self.xfraction, (height/self.nvert) * self.yfraction)
        self.plotA.axes(pageposition = (x,y), pagesize=size)
        self.plotA.draw(self)
        #restore previous settings
        self.plotA.axes(pageposition = oposition, pagesize = osize, centered = ocenter)
        self.plotA.finish_axes()
            
    def drawplotB(self):
        oposition = self.plotB.axes.pageposition
        osize = self.plotB.axes.pagesize
        ocenter = self.plotB.axes.centered
        width = self.page.width
        height = self.page.height
        x = (width/self.nhoriz) + (width * self.xoffset)
        y = (height/self.nvert) - ((height/self.nvert) * self.yoffset) 
        size = ((width/self.nhoriz) * self.xfraction, (height/self.nvert) * self.yfraction)
        self.plotB.axes(pageposition = (x,y), pagesize=size)
        self.plotB.draw(self)
        #restore previous settings
        self.plotB.axes(pageposition = oposition, pagesize = osize, centered = ocenter)
        self.plotB.finish_axes()



class Canvas4UP(Canvas2UP):
    """Canvas object for creating 4 Plots per page.
    
    Comments:
        * Do NOT use the centered option with axes when using Canvas2UP
           or Canvas4UP
    """    
    def __init__(self, plotA=None, plotB=None, plotC=None, plotD=None, **keywords):
        self.plotC, self.plotD = plotC, plotD
        Canvas2UP.__init__(self, plotA, plotB, **keywords)
        
    def setup_page(self):
        self.page(width = 5000, height = 5000)  

    def set_defaults(self):
        Canvas2UP.set_defaults(self) 
        self.setoptions(
            nvert = 2,
            nhoriz = 2,
            INFOMODULE = ("disipyl.optioninfo.canvas", "Canvas4UP"),
        )
        
    def drawplot(self):
        if not self.plotA:
            raise DislinException("Must specify at least one plot for Canvas4UP")
        self.drawplotA()
        if self.plotB:
            self.drawplotB()
        if self.plotC:
            self.drawplotC()
        if self.plotD:
            self.drawplotD()
            
    def drawplotC(self):
        oposition = self.plotC.axes.pageposition
        osize = self.plotC.axes.pagesize
        ocenter = self.plotC.axes.centered
        width = self.page.width
        height = self.page.height
        x = width * self.xoffset
        y = height - ((height/self.nvert) * self.yoffset) 
        size = ((width/self.nhoriz) * self.xfraction, (height/self.nvert) * self.yfraction)
        self.plotC.axes(pageposition = (x,y), pagesize=size)
        self.plotC.draw(self)
        #restore previous settings
        self.plotC.axes(pageposition = oposition, pagesize = osize, centered = ocenter)
        self.plotC.finish_axes()
            
    def drawplotD(self):
        oposition = self.plotD.axes.pageposition
        osize = self.plotD.axes.pagesize
        ocenter = self.plotD.axes.centered
        width = self.page.width
        height = self.page.height
        x = (width/self.nhoriz) + (width * self.xoffset)
        y = height - ((height/self.nvert) * self.yoffset) 
        size = ((width/self.nhoriz) * self.xfraction, (height/self.nvert) * self.yfraction)
        self.plotD.axes(pageposition = (x,y), pagesize=size)
        self.plotD.draw(self)
        #restore previous settings
        self.plotD.axes(pageposition = oposition, pagesize = osize, centered = ocenter)
        self.plotD.finish_axes()



class CanvasMultiPlot(Canvas):
    """Canvas object for creating multiple plots per page.
    
    The plots are created from a list of plots and are scaled according to
    their settings.
    
    Comments:
        * Do NOT use the 'centered' option with axes when using CanvasMultiPlot
    """
    def __init__(self, plotlist, **keywords):
        Object.__init__(self,**keywords)
        self.page = Page()
        self.plotlist = plotlist
        self.objects = []
        self.dispre = []
        self.dispost = []
        self.setup_page()
        
    def set_defaults(self):
        Canvas.set_defaults(self)
        self.setoptions(
            xoffset = 0.08,
            yoffset = 0.08,
            xfraction = 0.80,
            yfraction = 0.80,  
            INFOMODULE = ("disipyl.optioninfo.canvas", "CanvasPlotList"),
        )    
        
    def setup_page(self):
        self.page(width = 5000, height = 2500)  
        
    def drawplot(self):
        if not self.plotlist:
            raise DislinException("Must specify at least one plot for CanvasPlotList")
        iplot = 0
        for plot in self.plotlist:
            plot.draw(self)
            plot.finish_axes()


#------------------------------------------------------------------------------
# Page Object

class Page(Object):
    """Abstraction for setting parameters relevant to DISLIN page settings."""
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)

    def set_defaults(self):
        self.setoptions(
            fillcolor = None,
            border = 0,
            width = 3000,
            height = 2200,
            scale = 1,
            scalingmode = 'down',
            INFOMODULE = ("disipyl.optioninfo.page", "Page"),
        )

    def configure(self):
        pdis.setPageSize(self.width,self.height)
        pdis.setScaling(self.scale)
        pdis.setScalingMode(self.scalingmode)
        if self.border:
            pdis.setBorder()
        if self.fillcolor:
            pdis.setPageFill(self.fillcolor)



#------------------------------------------------------------------------------
# PlotObject -- Base class for plots

class PlotObject(Object):
    """Base class for all plot objects.
    
    Comments:
        * A PlotObject minimally has an axis sytem (axes).  It may additionally have
            a Title object and arbitrary data objects (the things drawn on the plot).
    
        * Two sequences (dispreaxis, dispostaxis) are provided to enable arbitrary
            dislin/pydislin calls at DISLIN level 1 (dispreaxis) or DISLIN level 2 
            (dispostaxis).  Elements of these sequences should be added as tuples of 
            form (func, (args)).  
    """
    def __init__(self, **keywords):
        """Base class for pxdislin drawings."""
        Object.__init__(self,**keywords)        
        self.axes = None
        self.title = None
        self.canvas = None      #points back to the canvas on which plot is drawn
        self.dataobjects = []
        self.dispreaxis = []
        self.dispostaxis = []

    def add(self, *obj):
        self.dataobjects.extend(obj)

    def cleanup(self):
        pass
        
    def finish_axes(self):
        pdis.end_axis()
        pdis.setHardwareFont()
        
    def set_defaults(self):  
        Object.set_defaults(self)
        self.__isexplorable = 0
        self.cursorpoints = [], []
        self.setoptions(
            TeXmode = 1,
            defaultcanvastype = Canvas,
            INFOMODULE=("disipyl.optioninfo.plotobject", "PlotObject"),
        )

    def configure(self):
        Object.configure(self)
        pdis.setColor('fore')
        pdis.setTeXMode(self.TeXmode)

    def draw(self,canvas=None):
        # some trickery to get plot object to create canvas on demand if it
        # is not assigned
        if not canvas:
            self.canvas = self.defaultcanvastype(self)
            self.canvas.draw()
        else:
            self.configure()
            self.drawdispreaxis()
            self.drawaxes()
            self.drawtitle()
            self.drawdataobjects()
            self.drawdispostaxis()
            self.cleanup()
            if self.__isexplorable:
                pdis.sendBuffer()
                xlist, ylist = pdis.getMouseClicks()
                self.cursorpoints[0].extend(xlist)
                self.cursorpoints[1].extend(ylist)

    def explore(self):
        self.__isexplorable = 1
        self.draw()
        self.__isexplorable = 0
        return self.cursorpoints
        
    def save(self, filename, format = 'postscript'):
        if self.canvas:
            self.canvas.save(filename, format)
        else:
            raise DislinException('Please assign PlotObject to a Canvas to save')
            
    def drawdispreaxis(self):
        """Execute any extra functions which should be done at DISLIN level 1"""
        for func, arg in self.dispreaxis:
            func(*arg)
            
    def drawdispostaxis(self):
        """Execute any extra functions which should be done at DISLIN levels 2 or 3"""
        for func, arg in self.dispostaxis:
            func(*arg)

    def drawaxes(self):
        if not self.axes:
            raise DislinException("Missing axis system")
        self.axes.draw()
        
    def drawtitle(self):
        if self.title:
            self.title.draw()

    def drawdataobjects(self):
        for obj in self.dataobjects:
            obj.draw()


        

#------------------------------------------------------------------------------
# Axis

class Axis(Object):
    """Abstraction of a single DISLIN axis."""
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)
        
    def set_defaults(self):
        Object.set_defaults(self)          
        self.setoptions(
            axisscaling = 'linear',
            labeljust = 'auto',
            labelorient = 'horizontal',
            labelposition = 'ticks',
            labelprecision = 'auto',
            labeltype = 'float',
            linecolor = -1,
            max = 1,
            min = -1,
            mylabels = [],
            name = '',
            namedist = 30,
            nameheight = 36,
            namejustify = 'center',
            noline = 0,
            textcolor = -1,
            ticklength = (None, None),
            ticknumber = 2,
            tickposition = 'sameaslabels',
            tickstart = -1,
            tickstep = 1,
            whichaxis = 'XYZ',
            INFOMODULE = ("disipyl.optioninfo.axis", 'Axis'),
        )

        
    def draw(self):
        self.configure()
        
    def configure(self):
        pdis.setAxisScaling(self.axisscaling, self.whichaxis)
        
        pdis.setLabelType(self.labeltype, self.whichaxis)
        if self.labeltype != 'none':
            pdis.setLabelOrientation(self.labelorient, self.whichaxis)
            pdis.setLabelPosition(self.labelposition, self.whichaxis)
            pdis.setLabelJustification(self.labeljust, self.whichaxis)
        
            if self.labeltype == 'mylab':
                if not self.mylabels:
                    raise DislinException('No MYLAB labels specified')
                pdis.setMyLabels(self.mylabels, self.whichaxis)
            else:
                if self.labelprecision == 'auto':
                    self.configlabelprecision()
                else:
                    pdis.setLabelDigits(self.labelprecision, self.whichaxis)
        
        pdis.setTickPosition(self.tickposition, self.whichaxis)
        pdis.setTickNumber(self.ticknumber, self.whichaxis)
        if self.ticklength[0]:
            pdis.setTickLength(self.ticklength[0],self.ticklength[1])
                    
        pdis.setAxisName(self.name, self.whichaxis)
        pdis.setAxisNameHeight(self.nameheight)
        pdis.setAxisNameJustification(self.namejustify, self.whichaxis)
        pdis.setAxisNameDistance(self.namedist, self.whichaxis)
        
        pdis.setAxisColors(self.linecolor, self.linecolor,
                          self.textcolor, self.textcolor, self.whichaxis)                                      
        
        if self.noline:
            pdis.noAxisLines(self.whichaxis)

    
    def configlabelprecision(self):
        m, e = math.frexp(self.tickstep)
        n = int(round(math.sqrt(abs(e))))
        if e >= 20:
            pdis.setLabelDigits(2, self.whichaxis)
            pdis.setLabelType('fexp', self.whichaxis)
        elif 20 > e > 1:
            pdis.setLabelDigits(0, self.whichaxis)
            pdis.setLabelType('float',self.whichaxis)
        else:
            if (n == 0) or (n == 1):
                pdis.setLabelDigits(2, self.whichaxis)
            elif n == 2:
                pdis.setLabelDigits(2*n, self.whichaxis)
            if n >= 3:
                pdis.setLabelDigits(2, self.whichaxis)
                pdis.setLabelType('fexp', self.whichaxis)


#------------------------------------------------------------------------------
# AxisSystem and AxisSystem2D        

class AxisSystem(Object):
    """Base class for axis systems."""
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)
        
    def set_defaults(self):
        Object.set_defaults(self)          
        self.setoptions( 
            centered = 0,
            clipped = 1,
            framethickness = 0,
            integerlabels = 0,
            pageorigin = (None, None),
            pageposition = (None, None),
            pagesize = (None, None),
            suppress = 0,
            INFOMODULE = ("disipyl.optioninfo.axis", 'AxisSystem'),
        )


    def configure(self):
        pdis.setFrameThickness(self.framethickness)
        if self.pageposition[0] is not None:
            # converting to int's seems to make jpython/java-dislin happier
            x, y = int(self.pageposition[0]), int(self.pageposition[1])
            pdis.setAxisPagePosition(x,y)
        if self.pageorigin[0] is not None:
            x, y = int(self.pageorigin[0]), int(self.pageorigin[1])
            pdis.setAxisPageOrigin(x,y)
        if self.pagesize[0] is not None:
            x, y = int(self.pagesize[0]), int(self.pagesize[1])
            pdis.setAxisPageSize(x,y)
        if not self.clipped:
            pdis.disableAxisClipping()
        if self.centered:
            pdis.centerAxis()          
        if self.suppress:
            pdis.noAxisSystem()
        if self.integerlabels:
            pdis.setIntegerLabels()
            
    def draw(self):
        pass           
                            
    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.resetParameter('setscl')            
        


class AxisSystem2D(AxisSystem):
    """Two-dimensional axis system.
    
    Comments:
        * If specified, xaxis and yaxis arguments should be Axis objects.
    """
    def __init__(self, xaxis=None, yaxis=None, **keywords):
        AxisSystem.__init__(self, **keywords)
        if xaxis is not None:   
            self.addX(xaxis)
        else:
            self.addX(Axis())
        if yaxis is not None:
            self.addY(yaxis)
        else:
            self.addY(Axis())
        
    def addX(self, xaxis):
        self.xaxis = xaxis
        if isinstance(self.xaxis, Axis):
            self.xaxis(whichaxis='X')
        else:
            raise DislinException('Non-valid X-Axis specified')

    def addY(self, yaxis):
        self.yaxis = yaxis
        if isinstance(self.yaxis, Axis):
            self.yaxis(whichaxis='Y')
        else:
            raise DislinException('Non-valid Y-Axis specified')            

    def set_defaults(self):
        AxisSystem.set_defaults(self)
        self.setoptions( \
            autoscaling = 0,
            autox = None,
            autoy = None,
            axisparts = ('all', 'all', 'ticks', 'ticks'),
            axistype = 'rectangular',
            gridlinestyle = 'dot',
            grids = 0,
            gridtype = 'square',
            ngridlines = (3, 3),
            originlines = None,
            originlinestyle = 'dash',
            squared = 0,
            INFOMODULE = ("disipyl.optioninfo.axis", 'AxisSystem2D'),
        )
        
                
    def configure(self):
        AxisSystem.configure(self)
        pdis.setAxisType(self.axistype)
        pdis.setAxisParts(*self.axisparts)
        if self.squared:
            self.squareup()        

            
    def draw(self):
        self.configure()
        try:
            if not self.xaxis or not isinstance(self.xaxis, Axis):
                raise DislinException('No x-axis specified or non-valid Axis object')
            if not self.yaxis or not isinstance(self.yaxis, Axis):
                raise DislinException('No y-axis specified or non-valid Axis object')
        except AttributeError:
            raise DislinException("Missing axis object for: %s" % str(self))
        self.xaxis.draw()
        self.yaxis.draw()
        self.postaxis()
        self.displayit()
        self.postdisplay()
        self.cleanup()
        
    def postaxis(self):
        if self.integerlabels:
            pdis.setIntegerLabels()
        
        
    def displayit(self):
        """Setup and make the actual calls to pydislin.axis2D."""
        xaxis, yaxis = self.xaxis, self.yaxis
        xlow,xhigh,xfirst,xstep = xaxis.min, xaxis.max,xaxis.tickstart,xaxis.tickstep
        ylow,yhigh,yfirst,ystep = yaxis.min, yaxis.max,yaxis.tickstart,yaxis.tickstep 
        
        if self.autoscaling:
            if self.autox:
                pdis.autoAxisScaling(self.autox, 'X')
            if self.autoy:
                pdis.autoAxisScaling(self.autoy, 'Y')
            dislin.graf(xlow,xhigh,xfirst,xstep, ylow,yhigh,yfirst,ystep)
            xlow, xhigh, xfirst, xstep = pdis.getAxisData('X')
            ylow, yhigh, yfirst, ystep = pdis.getAxisData('Y')
            self.xaxis(min=xlow,max=xhigh,tickstart=xfirst,tickstep=xstep)
            self.yaxis(min=ylow,max=yhigh,tickstart=yfirst,tickstep=ystep)
        else:    
            pdis.axis2D(xlow,xhigh,xfirst,xstep, ylow,yhigh,yfirst,ystep)
            
    def postdisplay(self):
        if self.grids:
            pdis.setLineStyle(self.gridlinestyle)
            ng = self.ngridlines
            pdis.setGrid(ng[0],ng[1],self.gridtype)
        if self.originlines:
            pdis.setLineStyle(self.originlinestyle)
            pdis.setOriginLines(self.originlines)
            
    def limits(self, xmin, xmax, ymin, ymax):
        """A convenience function for quickly setting limits of x and y axes."""
        self.xaxis(min = xmin, max = xmax)
        self.yaxis(min = ymin, max = ymax)
        
    def ticks(self, xstart, xstep, ystart, ystep):
        """A convenience function for quickly setting tickstart/step of x and y axes."""
        self.xaxis(tickstart = xstart, tickstep = xstep)
        self.yaxis(tickstart = ystart, tickstep = ystep)
            
    def squareup(self):
        """Causes x,y scaling to be equal."""
        if allExist(*self.pagesize):
            sizex, sizey = self.pagesize
            size = min(sizex,sizey)
        else:
            sizex, sizey = pdis.getPageSize()
            size = min(sizex,sizey) - 500
        dx = self.xaxis.max - self.xaxis.min
        dy = self.yaxis.max - self.yaxis.min
        if dx < dy:
            psize = (size * (dx/dy), size)
        else:
            psize = (size, size * (dy/dx))
        return pdis.setAxisPageSize(psize[0], psize[1])
          
            
            
#------------------------------------------------------------------------------
# Title

class Title(Object):
    """Abstraction of Title object for disipyl plots."""
    def __init__(self, text="", **keywords):
        Object.__init__(self, **keywords)
        self.setdata(
            text = text
        )

    def set_defaults(self):
        Object.set_defaults(self)
        self.setoptions(
            color = 'fore',
            height = 48,
            justify = 'center',
            offset = 0,
            position = 'above',
            INFOMODULE = ('disipyl.optioninfo.page', 'Title'),
       )

    def configure(self):
        pass
                
    def draw(self):
        self.configure()
        pdis.setColor(self.color)
        pdis.setTitle(self.text, self.height,self.justify, 
                      self.position, self.offset)
        
    

#------------------------------------------------------------------------------
# Drawable objects

class DrawObject(Object):
    """Base class for most disipyl objects which are drawn on/in plots.
    
    Comments:
        * If specified disfunc should be a dislin/disipyl function which
            actually draws the object.
        * Arguments for disfunc should generally be set as the last step
            in the configure method.
    """
    def __init__(self, disfunc=None, **keywords):
        Object.__init__(self, **keywords)
        self.func = disfunc
        self.args = None

    def set_defaults(self):
        Object.set_defaults(self)
        self.setoptions(
            color = 'fore',
            ucoords = 1,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'DrawObject'),
        )
                
    def configure(self):
        # Almost everything has a color
        pdis.setColor(self.color)
        # Do this for derived classes
        # self.args = ...   
        
    def draw(self):
        self.configure()
        if self.func:
            if not allExist(*self.args):
                raise DislinException('Missing arguments in DrawObject: %s' % str(self))
            self.func(*self.args)
        self.cleanup()
        
    def cleanup(self):
        # Cleanup any last settings
        pass
            
#------------------------------------------------------------------------------
# Drawable Objects                    

class Text(DrawObject):
    """Text object. Either character string or number."""
    def __init__(self, text="", **keywords):
        disfunc = pdis.drawText
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata(text = text)
        
    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions(
            TeX = 0,
            font = 'default',
            fonttype = 'hardware',
            height = 36,
            justify = 'left',
            position = (0, 0),
            rotation = 0,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'Text'),
       )

    def configure(self):
        DrawObject.configure(self)
        pdis.setTextHeight(self.height)
        pdis.setTextAngle(self.rotation)
        pdis.setFont(self.font, self.fonttype)
        pdis.setTextJustification(self.justify)
        self.args = (self.text, self.position[0],self.position[1], \
                     self.ucoords, self.TeX)
                     
#------------------------------------------------------------------------------
        
class Symbol(DrawObject):
    """Two-dimensional symbol object."""
    def __init__(self, x=None,y=None,**keywords):
        disfunc = pdis.drawSymbol
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            position = (x,y),
        )
        

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            rotation = 0,
            shape = 'circle',
            size = 20,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'Symbol'),
       )


    def configure(self):
        DrawObject.configure(self)
        pdis.setSymbolSize(self.size)
        pdis.setSymbolRotation(self.rotation)
        self.args = (self.position[0],self.position[1],self.shape,self.ucoords)
        

class LabeledSymbol(Symbol):
    """Symbol object, drawn with an offset label."""
    def __init__(self, x=0, y=0, **keywords):
        Symbol.__init__(self, x, y, **keywords)
        
    def set_defaults(self):
        Symbol.set_defaults(self)
        self.setoptions( 
            labelheight = 25,
            labeltext = '',
            xoffset = -40,
            yoffset = 40,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'LabeledSymbol'),
        )
        
        
    def draw(self):
        Symbol.draw(self)
        x,y = self.position
        px,py,pz = pdis.userToPlot(x,y)
        label = Text(text=self.labeltext, height=self.labelheight,\
                     position = (px + self.xoffset, py + self.yoffset),
                     ucoords = 0)
        label.draw()
     
#------------------------------------------------------------------------------
       
class Line(DrawObject):
    """Line object.
    """
    def __init__(self, startx=0, starty=0, endx=0, endy=0,**keywords):
        disfunc = pdis.drawLine
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            start = (startx, starty),
            end = (endx, endy),
        )
        

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            style = 'solid',
            width = 1,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'Line'),
       )


    def configure(self):
        DrawObject.configure(self)
        pdis.setLineWidth(self.width)
        pdis.setLineStyle(self.style)
        self.args = (self.start[0],self.start[1],\
                     self.end[0], self.end[1], self.ucoords)
                     
    
    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1)                     
                     

class Vector(Line):
    """Two-dimensional vector object."""
    def __init__(self, startx=0, starty=0, endx=0, endy=0,**keywords):
        disfunc = pdis.drawVector
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata(
            start = (startx, starty),
            end = (endx, endy),
        )
        
    def set_defaults(self):
        Line.set_defaults(self)
        self.setoptions( 
            vectortype = 'normal',
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'Vector'),
       )


    def configure(self):
        Line.configure(self)
        self.args = (self.start[0],self.start[1],\
                     self.end[0], self.end[1], self.vectortype, self.ucoords)
#------------------------------------------------------------------------------
                     
class VectorField(DrawObject):
    """Two-dimensional vector field."""
    def __init__(self, startx=0,starty=0,endx=0,endy=0, **keywords):
        disfunc = pdis.drawVectorField
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata(
            startx = startx,
            starty = starty,
            endx = endx,
            endy = endy,
        )
 

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions(
            style = 'solid',
            vectortype = 'normal',
            width = 1,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'VectorField'),
        )


    def configure(self):
        DrawObject.configure(self)
        pdis.setLineWidth(self.width)
        pdis.setLineStyle(self.style)
        self.args = (self.startx,self.starty,
                     self.endx, self.endy, self.vectortype)   
                     
    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1)                           
        
#------------------------------------------------------------------------------
                     
class Curve(DrawObject):
    """Two-dimensional curve.
    
    Comments:
        * Set the option 'symbols' to a positive integer n, to draw
            curve line plus every n-th symbol.  Set to zero to
            draw only curve lines.  Set the negative integer to
            draw only every n-th symbol (no curve lines).
    """
    def __init__(self, xlist=[], ylist=[], **keywords):
        disfunc = pdis.drawCurve
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist,
        )

        
    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            interpolation = 'linear',
            splinenumber = 200,
            splineorder = 3,
            style = 'solid',
            symbols = 0,
            symbolshape = 'triangle',
            symbolsize = 30,
            width = 1,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'Curve'),
       )


    def configure(self):
        DrawObject.configure(self)
        pdis.setCurveWidth(self.width)
        pdis.setLineStyle(self.style)
        pdis.setCurveInterpolation(self.interpolation)
        pdis.setSplineMode(self.splineorder,self.splinenumber)
        pdis.setCurveSymbols(self.symbols, self.symbolshape, self.symbolsize)
        self.args = (self.xlist, self.ylist)

    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1)         


class AreaBetweenCurves(DrawObject):
    """Representation of the area between two curves.
    
    Arguments in constructor should be Curve objects.
    """
    def __init__(self, curve0=None, curve1=None, **keywords):
        disfunc = pdis.drawShadedCurve
        DrawObject.__init__(self,disfunc,**keywords)
        self.curve0 = curve0
        self.curve1 = curve1

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            pattern = 'diagonal1',
            outline = 1,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'AreaBetweenCurves'),
        )

        
    def configure(self):
        DrawObject.configure(self)
        x1,y1 = self.curve0.xlist, self.curve0.ylist  
        x2,y2 = self.curve1.xlist, self.curve1.ylist
        pdis.setShadingPattern(self.pattern, self.outline)
        self.args = (x1,y1,x2,y2,)

    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1) 

#------------------------------------------------------------------------------
    
class ErrorBars(DrawObject):
    """Class for drawing error bars."""
    def __init__(self, xlist=[], ylist=[], elow=[], ehigh=[], **keywords):
        disfunc = pdis.drawErrorBars
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            elow = elow,
            ehigh = ehigh,
        )


    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            horizontalbars = 0,
            marker = None,
            markersize = 30,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'ErrorBars'),
        )


    def configure(self):
        DrawObject.configure(self)
        self.args = (self.xlist, self.ylist, self.elow, self.ehigh,
                     self.marker, self.markersize, self.horizontalbars )        
        
#------------------------------------------------------------------------------
# Geometric figures

class GeometricFigure(DrawObject):
    """Base class for two-dimensional geometric figures."""
    def __init__(self, **keywords):
        disfunc = None
        DrawObject.__init__(self,disfunc,**keywords)

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            linestyle = 'solid',
            outline = 1,
            position = (0, 0),
            pattern = None,
            INFOMODULE = ('disipyl.optioninfo.drawobject', 'GeometricFigure'),
        )


    def configure(self):
        DrawObject.configure(self)
        pdis.setShadingPattern(self.pattern, self.outline)
        pdis.setLineStyle(self.linestyle)      
        
    def cleanup(self):
        pdis.setLineStyle('solid')
        pdis.setLineWidth(1) 


class Rectangle(GeometricFigure):   
    def __init__(self, x=0, y=0, width=0, height=0, **keywords):
        disfunc = pdis.drawRectangle
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            position = (x,y),
            width = width,
            height = height,
        )

    def set_defaults(self):
        GeometricFigure.set_defaults(self)
        self.setoptions( 
            width = 0,
            height = 0,
            rounding = 0,
        )


    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.position[0], self.position[1],
                     self.width, self.height, self.rounding, self.ucoords)            
        

class Circle(GeometricFigure):   
    def __init__(self, x=0, y=0, radius=0, **keywords):
        disfunc = pdis.drawCircle
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            position = (x,y),
            radius = radius,
        )

    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.position[0], self.position[1],self.radius, self.ucoords)


class Ellipse(GeometricFigure):   
    def __init__(self, x=0, y=0, a=0, b=0, **keywords):
        disfunc = pdis.drawEllipse
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            position = (x,y),
            a = a,
            b = b,
        )

        
    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.position[0], self.position[1],
                    self.a, self.b, self.ucoords)        


class EllipticArc(GeometricFigure):
    def __init__(self, x=0, y=0, a=0, b=0,alpha=0, beta=0, theta=0, **keywords):
        disfunc = pdis.drawEllipticArc
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            position = (x,y),
            a = a,
            b = b,
            alpha = alpha, beta = beta, theta = theta
        )
            
    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.position[0], self.position[1],
                    self.a, self.b, self.alpha, self.beta, self.theta, self.ucoords)  


class GeometricPie(GeometricFigure):
    def __init__(self, x=0, y=0, radius=0, alpha=0, beta=0, **keywords):
        disfunc = pdis.drawPie
        DrawObject.__init__(self,disfunc,**keywords)        
        self.setdata(
            position = (x,y),
            radius = radius,
            alpha = alpha, beta = beta
        )


    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.position[0], self.position[1],
                    self.radius, self.alpha, self.beta, self.ucoords)


class Polygon(GeometricFigure):
    def __init__(self, xlist=[], ylist=[], **keywords):
        disfunc = pdis.drawEllipse
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist
        )
            

    def configure(self):
        GeometricFigure.configure(self)
        self.args = (self.xlist, self.ylist, self.ucoords)  
        

#------------------------------------------------------------------------------
# Bar graph Objects

class Bars(DrawObject):
    """Bar graph object."""
    def __init__(self, xlist=[], ystart=0, yend=0, **keywords):
        disfunc = pdis.drawBar
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata(
            xlist = xlist,
            ystart = ystart,
            yend = yend
        )

        
    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            bordercolor = -1,
            color = -1,
            direction = 'vertical',
            filled = 0,
            groupbars = 0,
            groupgap = 1,
            labelcolor = -1,
            labeldigits = 1,
            labelmode = 'none',
            labelposition = 'auto',
            look3D = (-1, 45),
            ngroups = 2,
            pattern = 'solid',
            position = 'none',
            sidecolor = -1,
            threeD = 0,
            topcolor = -1,
            width = 0.75,
            widthmode = 'fixed',
        )


    def configure(self):
        if self.filled:
            pdis.setShadingPattern(self.pattern)
        if self.threeD:
            if self.direction[:5] == 'horiz':
                pdis.setBarType('horizontal3D')
            else:
                pdis.setBarType('vertical3D')
            pdis.setBar3DLook(*self.look3D)
        else:
            pdis.setBarType(self.direction)
        
        pdis.setBarWidth(self.width)
        # INVESTIGATE
        # Setting bar width mode (dislin.barmod) seems to cause crashes.
        #pdis.setBarWidthMode(self.widthmode)
        # INVESTIGATE
        # Setting bar position variable actually seems to screw things up.
        # Even if the setting is 'NONE' it seems to force bars to align with ticks.
        #pdis.setBarPosition(self.position)
        if self.groupbars:
            pdis.setBarGroups(self.ngroups, self.groupgap)
        pdis.setBarColor(self.color, self.sidecolor, self.topcolor)
        pdis.setBarBorderColor(self.bordercolor)
        pdis.setBarLabelStyle(self.labelmode, self.labelposition,
                              self.labeldigits, self.labelcolor)
        
        self.args = (self.xlist, self.ystart, self.yend)
        


class HistoBars(Bars):
    """Similar to Bars object but bars always start from min(yaxis)"""
    def __init__(self, xlist=[], yend=0, **keywords):
        ystart = None
        if xlist:
            ystart = [0]*len(xlist)
        Bars.__init__(self, xlist, ystart, yend, **keywords)

    def set_defaults(self):
        Bars.set_defaults(self)   
        self.setoptions( 
            position = 'axis',
        )

#------------------------------------------------------------------------------
# Pie Chart Objects

class PieChart(DrawObject):
    """Pie chart object.
    
    * PieChart should be handed to a Canvas object for drawing.
    * PieChart should NOT be used in conjunction with a regular axis system.
    
    
    Comments: This is very rough, and could be improved upon to provide
      all the flexibility that DISLIN offers. However, I very rarely use pie
      charts.  Perhaps a motivated user will do it. Hint, hint!
    """
    def __init__(self, xlist=[], **keywords):
        disfunc = pdis.drawPieChart
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata( xlist = xlist)
        
    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            appearance = '2D',
            arrowtype = 'broken',
            autooffset = 0,
            bordercolor = -1,
            cycle = 'both',
            labelcolor = -1,
            labeldigits = 1,
            labeljustify = 'center',
            labelmode = 'percent',
            labelposition = 'internal',
            labels = [],
            sidecolors = [],
            thickness3D = 0.2,
            topcolors = [],
            viewangle3D = 45,
        )

        
    def configure(self):
        if self.autooffset:
            pdis.setPieAutoOffset()
        pdis.setPieAppearance(self.cycle, self.appearance)
        pdis.setPieLabelStyle(self.labelmode, self.labelposition, 
                              self.labeljustify, self.labeldigits, self.labelcolor)
        pdis.setPieOptions3D(self.thickness3D, self.viewangle3D)  
        pdis.setPieArrowType(self.arrowtype)
        pdis.setPieBorderColor(self.bordercolor)
        if len(self.topcolors) and len(self.sidecolors):
            pdis.setPieSliceColors(self.topcolors, self.sidecolors)
        self.args = (self.xlist,self.labels)
        
    def draw(self, canvas=None):
        DrawObject.draw(self)
  
        
#------------------------------------------------------------------------------
# Legend

class Legend(DrawObject):
    """Representation of DISLIN legends.
    
    * Legend item options can be set automatically by calls to dislin.curve
        or can be customized to user preferences.
    """
    def __init__(self, **keywords):
        disfunc = pdis.drawLegend
        DrawObject.__init__(self, disfunc, **keywords)

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            auto = 1,
            colors = [],
            framewidth = 1,
            linestyles = [],
            linewidths = [],
            patterns = [],
            position = 'pageUR',
            symbols = [],
            textlines = [],
            title = 'Legend',
        )


    def configure(self):
        pdis.setColor('fore')
        if not self.textlines:
            raise DislinException('No textlines defined in Legend object.')
        nlines = len(self.textlines)
        maxlen = max([len(i) for i in self.textlines])
        pdis.initializeLegend(len(self.textlines), maxlen)
        pdis.setLegendTitle(self.title)
        pdis.setLegendFrameWidth(self.framewidth)
        for i in range(nlines):
            pdis.addLegendLine(self.textlines[i], i+1)
        if len(self.linewidths):
            widths = self.linewidths
        else:
            widths = [1] * nlines
        if len(self.linestyles):
            styles = self.linestyles
        else:
            styles = [None]*nlines
        if len(self.symbols):
            symbols = self.symbols
        else:
            symbols = [None] * nlines
        if len(self.patterns):
            patterns = self.patterns
        else:
            patterns = [None] * nlines
        if len(self.colors):
            colors = self.colors
        else:
            colors = [-1] * nlines
        if not self.auto:
            for i in range(len(self.textlines)):
                pdis.setLegendPattern(i+1, widths[i], styles[i], symbols[i],
                                      colors[i], patterns[i])
        self.args = (self.position,)
        
#------------------------------------------------------------------------------
# DrawGroups

class DrawGroup(Object):
    """Container object for DrawObject instances with similar options"""
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)
        self.objects = []
        
    def add(self, *obj):
        self.objects.extend(obj)
               
    def set_defaults(self):
        Object.set_defaults(self)
        self.setoptions( 
            color = 'fore',
        )
        
    def configure(self):
        for object in self.objects:
            for option in self.__options__:
                if option in object.__options__:
                    object.__setattr__(option, self.__getattribute__(option))
                    
    def draw(self):
        self.configure()
        for obj in self.objects:
            obj.draw()


class SymbolGroup(DrawGroup):
    """Container for two-dimensional symbols with similar options."""
    def __init__(self, xlist=[], ylist=[], **keywords):
        DrawGroup.__init__(self, **keywords)
        self.objects = [Symbol(x,y) for x,y in zip(xlist,ylist)]
        
    def set_defaults(self):
        DrawGroup.set_defaults(self)
        self.setoptions( 
            rotation = 0,
            shape = 'circle',
            size = 20,
            ucoords = 1,
        )


class LabeledSymbolGroup(SymbolGroup):
    def __init__(self, xlist=[], ylist=[], labels=[], **keywords):
        DrawGroup.__init__(self, **keywords)
        self.objects = [LabeledSymbol(x,y) for x,y in zip(xlist,ylist)]
        if labels:
            for i in range(len(labels)):
                self.objects[i](labeltext = labels[i])
        
    def set_defaults(self):
        SymbolGroup.set_defaults(self)
        self.setoptions( 
            rotation = 0,
            shape = 'circle',
            size = 20,
            ucoords = 1,
        )


        

class LineGroup(DrawGroup):        
    """Container for two-dimensional lines with similar options. 
    
    Also works with Curve objects.
    """
    def __init__(self, startx=None, starty=None, endx=None, endy=None, **keywords):
        DrawGroup.__init__(self, **keywords)
        if allExist(startx, starty, endx, endy):
            starts = zip(startx, starty)
            ends = zip(endx, endy)
            self.objects = [Line(start=i[0], end=i[1]) for i in zip(starts, ends)]        
        
    def set_defaults(self):
        DrawGroup.set_defaults(self)
        self.setoptions( 
            width = 1,
            style = 'solid',
            ucoords = 1,
        )



class GeometricGroup(DrawGroup, GeometricFigure):
    """Container for two-dimensional geometric figures with similar options.
    
    """
    def set_defaults(self):
        GeometricFigure.set_defaults(self)
        



#------------------------------------------------------------------------------
##  $Log: pxdislin.py,v $
##  Revision 1.8  2002/04/23 18:18:30  pmagwene
##  Added ability to write out PNG/PDF as string buffers
##
##  Revision 1.7  2002/04/23 16:24:48  pmagwene
##  Fixed a problem with the INFOMODULE option which prevented pickling
##
##  Revision 1.6  2002/04/23 14:52:52  pmagwene
##  more optioninfo
##
##  Revision 1.5  2002/04/17 04:43:44  pmagwene
##  doc update
##
##  Revision 1.4  2002/04/14 21:50:01  pmagwene
##  doc update
##
##  Revision 1.3  2002/04/14 05:11:56  pmagwene
##  Modified the format of INFOMODULE to take both a module name and a class name
##
##  Revision 1.2  2002/04/12 01:10:48  pmagwene
##  Implemented "mylabels" options for Axis submitted by Reggie Duggard
##
##  Revision 1.1.1.1  2002/04/11 15:19:00  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.7  2002/03/14 03:21:12  pmagwene
##  Futzing around a bit with __options__
##
##  Revision 1.6  2002/03/09 21:34:35  pmagwene
##  added quickplots
##
##  Revision 1.5  2002/03/09 15:23:42  pmagwene
##  yet more version stuff
##
##  Revision 1.4  2002/03/09 15:18:21  pmagwene
##  Version juggling code added
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
##  Revision 1.17  2001/06/05 12:35:19  pmagwene
##  *** empty log message ***
##
##  Revision 1.16  2001/04/20 02:53:53  pmagwene
##  minor tweaks
##
##  Revision 1.15  2001/04/17 20:05:09  pmagwene
##  *** empty log message ***
##
##  Revision 1.14  2001/04/05 14:44:20  pmagwene
##  Messed around with __setattr__ in base Object
##  Added miscplots.py - for useful plots that don't deserver to be
##   in plots.py (which should be rserved for the most basic plots)
##
##  Revision 1.13  2001/03/27 03:00:01  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.12  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
##  Revision 1.11  2001/03/25 20:19:57  pmagwene
##  Modified demos to return Canvas rather than plot objects
##  Renamed tkdislin -> tkdisipyl
##  Fleshed out tkdisipyl
##  Added cool 2D and 3D demos to tkdisipyl
##
##  Revision 1.10  2001/03/23 19:08:30  pmagwene
##  *** empty log message ***
##
##  Revision 1.9  2001/03/23 00:22:51  pmagwene
##  Some tweaknig of AxisSystem2D, Axis, and related functions to give better defaults.
##  Started mlabplot -- a set of functions with similar functionality to their counterparts in MATLAB.
##
##  Revision 1.8  2001/03/21 18:24:56  pmagwene
##  Fixes to Bars object
##
##  Revision 1.7  2001/03/21 17:41:58  pmagwene
##  Cleanup and doc strings.  Minor fixes.
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
