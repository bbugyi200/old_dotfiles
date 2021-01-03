#!/usr/bin/env python
""" A module for interfacing disipyl and Tkinter

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
##  $Id: tkdisipyl.py,v 1.1.1.1 2002/04/11 15:19:08 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""

#------------------------------------------------------------------------------
import sys, copy, random, os, os.path, string
import Tkinter
import tkFileDialog

#------------------------------------------------------------------------------

import pxdislin, plots
from pxdislin import DislinException
from utilities import Loop, pickleObject, unpickleObject

#------------------------------------------------------------------------------

filetype_dict = {'.ps':'postscript',
                 '.eps': 'color postscript',
                 '.tif': 'tiff',
                 '.png': 'png',
                 '.wmf': 'wmf',
                 '.pdf': 'pdf',
                 }
                 
#------------------------------------------------------------------------------
# Command is useful for flexible Tkinter callbacks - See Grayson's book
# --Attributed to Timothy R. Evans

class Command:
    """A class for making Tkinter callbacks more flexible."""
    def __init__(self, func, *args, **kw):
        self.func = func
        self.args = args
        self.kw = kw

    def __call__(self, *args, **kw):
        args = self.args + args
        kw.update(self.kw)
        self.func(*args, **kw)                 


#------------------------------------------------------------------------------
# tkFrame

class tkFrame(Tkinter.Frame):
    """A Frame object modified for drawing disipyl Canvas objects.
    
    Comments:
        * Will also except a descendant of PlotObject and instantiate a
          disipyl Canvas object as needed.
    """
    def __init__(self, master, canvas, width=500, height=500, bg='', **kw):
        Tkinter.Frame.__init__(self, master, width=width, height = height, bg=bg, **kw)
        
        if isinstance(canvas, pxdislin.Canvas):
            self.canvas = canvas
        elif isinstance(canvas, pxdislin.PlotObject):
            self.canvas = pxdislin.Canvas(canvas)
            self.canvas.plot.axes(centered = 1)
        else:
            raise DislinException('Non-Canvas or Plot object passed to tkdislin.tkFrame')
        self.get_init_values()           
        self.canvas.page(scalingmode='full')    # causes page to be scaled up/down depending on Frame
        
        self.bind('<Configure>', self.redraw)
        self.bind('<Expose>', self.redraw)
        
    
    def __del__(self):
        self.canvas(windowID = self._origid, windowtype=self._origtype)
            
    def get_init_values(self):
        self._origid = self.canvas.windowID
        self._origtype = self.canvas.windowtype
     
    def draw_canvas(self):
        if not self.winfo_ismapped():
            return
        try:
            handle = self.winfo_id()
        except:
            return
        self.canvas(windowID = handle, windowtype = 'window')
        self.canvas.draw()
     
    def redraw(self, event=None):
        self.draw_canvas()
        
    def update(self):
        Tkinter.Frame.update(self) 
        self.draw_canvas()




class tkFrame3D(tkFrame):
    """A Frame object with some additional functionality for 3D plots."""
    def __init__(self,master, canvas, width=500, height=500, bg='', **kw):
        tkFrame.__init__(self, master, canvas, width=width, height=height, bg=bg, **kw)
        self.canvas.page(width = 3000, height = 3000)

    def get_init_values(self):
        tkFrame.get_init_values(self)
        self._oview = self.canvas.plot.axes.viewpoint
        self._orot = self.canvas.plot.axes.camerarotation

    def rot_left(self, event=None):
        self.canvas.plot.rotate_left()
        self.redraw()
    
    def rot_right(self, event=None):
        self.canvas.plot.rotate_right()
        self.redraw()
        
    def rot_up(self, event=None):
        self.canvas.plot.rotate_up()
        self.redraw()
        
    def rot_down(self, event=None):
        self.canvas.plot.rotate_down()
        self.redraw()
        
    def tilt_left(self, event=None):
        self.canvas.plot.tilt_left()
        self.redraw()
        
    def tilt_right(self, event=None):
        self.canvas.plot.tilt_right()
        self.redraw()        
        
    def zoom_in(self, event=None):
        self.canvas.plot.zoom_in()
        self.redraw()        
      
    def zoom_out(self, event=None):
        self.canvas.plot.zoom_out()
        self.redraw()       

    def reset_view(self, event=None):
        self.canvas.plot.axes(viewpoint = self._oview)
        self.canvas.plot.axes(camerarotation = self._orot)
        self.redraw()          



#------------------------------------------------------------------------------
# classes for working in IDLE

def hasPlot3D(canvas):
    if isinstance(canvas, pxdislin.Canvas):
        if not hasattr(canvas, 'plot'):
            return 0
        if isinstance(canvas.plot, plots.Plot3D):
            return 1
    elif isinstance(canvas, plots.Plot3D):
        return 1
    else:
        return 0



class Diddle(tkFrame, tkFrame3D):
    """An object which facilitates visualization of disipyl plots in IDLE.
    
    Comments:
        * This constructs a Toplevel Tkinter widget which contains
            a tkFrame (or tkFrame3D) object (see above).
        * This object also provides a useful file menu for pickling and 
            unpickling plots and for saving plots in various graphics formats.
        * If the plot is derived from plots.Plot3D then a 3D navigation bar
           will also be constructed
    """
    
    def __init__(self, canvas, width=640, height=480, bg='', **kw):
        self.mainframe = Tkinter.Frame(None, bd=1,relief=Tkinter.SUNKEN)
        self.mainframe.pack(expand=1,fill=Tkinter.BOTH, anchor='center')
        self.mainframe.master.title('dispyl IDLE Window: Click plot to refresh!')

        if hasPlot3D(canvas):
            tkFrame3D.__init__(self, self.mainframe, canvas, width=600, height = 600, **kw)
            tkFrame3D.get_init_values(self)
            self.setup_navframe()
        else:            
            tkFrame.__init__(self, self.mainframe, canvas, width=640, height = 480, **kw)         

        self.config(bd=2, relief="sunken")       
        self.bind('<Button-1>',self.redraw)
        self.setup_menu()
        self.pack(expand=1,fill=Tkinter.BOTH, anchor='center')
        self.savedir = None
        self.saveext = None

            
    def setup_navframe(self):
        navframe = NavFrame3D(self.master, bd = 2, relief='raised')
        navframe.btrotleft.config(command = self.rot_left)
        navframe.btrotright.config(command = self.rot_right)
        navframe.btrotup.config(command = self.rot_up)
        navframe.btrotdown.config(command = self.rot_down)        
        navframe.btzoomout.config(command = self.zoom_out)
        navframe.btzoomin.config(command = self.zoom_in)        
        navframe.btreset.config(command = self.reset_view)
        navframe.bttiltleft.config(command = self.tilt_left)
        navframe.bttiltright.config(command = self.tilt_right)
        navframe.pack(fill=Tkinter.BOTH, anchor='n')

    
    def setup_menu(self):
        menu = Tkinter.Menu(self)
        menu.config(bd=2, relief='ridge')
        drawmenu = Tkinter.Menu(menu)
        menu.add_cascade(label="File", menu=drawmenu, underline=0)
        drawmenu.add_command(label="Pickle Plot", command = self.pickle_plot, underline=0)
        drawmenu.add_command(label="Unpickle Plot", command = self.unpickle_plot, underline=0)
        drawmenu.add_command(label="Save Image", command = self.save_plot_image, underline=0)       
        self.mainframe.master.config(menu=menu)        
        
    def pickle_plot(self):
        currdir = os.getcwd()
        sname = tkFileDialog.asksaveasfilename(defaultextension = ".pickle",
            filetypes = [('Pickled objects','*.pickle'),
                        ('All', '*.*')],
                        title = 'Pickle Plot',
                        parent=self,
                        initialdir = currdir)
        if not sname:
            return
        oldID = self.canvas.windowID
        self.canvas(windowID = 0, windowtype = 'none')
        sname = os.path.expanduser(sname)
        pickleObject(self.canvas, sname)
        self.canvas(windowID = oldID, windowtype = 'window')
       
        
    def unpickle_plot(self):
        currdir = os.getcwd()
        oname = tkFileDialog.askopenfilename(defaultextension = ".pickle",
            filetypes = [('Pickled objects','*.pickle'),
                        ('All', '*.*')],
                        title = 'UnPickle disipyl Plot',
                        parent=self,
                        initialdir = currdir)
        if not oname:
            return
        oname = os.path.expanduser(oname)
        plot = unpickleObject(oname)
        if plot is not None:
            if isinstance(plot, pxdislin.Canvas):
                self.canvas = plot
                self.redraw()
        else:
            raise DislinException("Unable to unpickle disipyl canvas")
        
    
    def save_plot_image(self):
        if self.savedir is None:
            currdir = os.getcwd()
        else:
            currdir = self.savedir
        if self.saveext is None:
            defext = ".ps"
        else:
            defext = self.saveext
        sname = tkFileDialog.asksaveasfilename(defaultextension = defext,
            filetypes = [('postscript','*.ps'),
                        ('color postscript', '*.eps'),
                        ('TIFF', '*.tif'),
                        ('PNG', '*.png'),
                        ('Windows Metafile', '*.wmf'),
                        ('PDF', '*.pdf')],
                        title = 'Save Plot As',
                        parent=self,
                        initialdir = currdir)
        if not sname:
            return
        sname = os.path.expanduser(sname)
        pth, ext = os.path.splitext(sname)
        filetype = filetype_dict[string.lower(ext)]
        self.canvas(fileoverwrite = 1)
        self.canvas.save(sname, format = filetype)
        self.savedir = os.path.split(pth)[0]
        self.saveext = ext

#------------------------------------------------------------------------------
# Plot navigation Frames

class NavFrame3D(Tkinter.Frame):
    """Frame with buttons for navigating 3D plots."""
    def __init__(self, master, **kw):
        Tkinter.Frame.__init__(self, master, **kw)
        self.setup_buttons()
        
    def setup_buttons(self):
        self.btzoomin = Tkinter.Button(self, text="Zoom In", width = 12, height = 1)
        self.btzoomout = Tkinter.Button(self, text="Zoom Out", width = 12, height = 1)
        self.btrotleft = Tkinter.Button(self, text="Rot Lt", width = 12, height = 1)
        self.btrotright = Tkinter.Button(self, text="Rot Rt", width = 12, height = 1)
        self.btrotup = Tkinter.Button(self, text="Rot Up", width = 12, height = 1)
        self.btrotdown = Tkinter.Button(self, text="Rot Down", width = 12, height = 1)
        self.bttiltleft = Tkinter.Button(self, text="Tilt Left", width = 12, height = 1)
        self.bttiltright = Tkinter.Button(self, text="Tilt Right", width = 12, height = 1)
        self.btreset = Tkinter.Button(self, text="Reset", width = 12, height = 1)
                       
        self.bttiltleft.grid(row=1, column =0, sticky='E')
        self.btrotup.grid(row=1, column = 1, sticky='N')
        self.bttiltright.grid(row=1, column =2, sticky='W')                   
        
        self.btrotleft.grid(row=2, column = 0,sticky='E')
        self.btreset.grid(row=2, column = 1, sticky='N')
        self.btrotright.grid(row=2,column=2,sticky='W')
        
        self.btzoomin.grid(row=3, column = 0,sticky='E')
        self.btrotdown.grid(row=3, column = 1, sticky='N')
        self.btzoomout.grid(row=3,column=2,sticky='W')    


#------------------------------------------------------------------------------
# Demo classes

            
class DemoFrame(tkFrame):
    """Base Demo class for demonstrating use of tkFrame."""
    def __init__(self, master, demoplots):
        tkFrame.__init__(self, master, canvas=demoplots[0], width=750, height = 550, bg='')
        self.pcount = 0                
        #setup a circular seq. of example plots (speeds drawing of plots up a little)
        self.examples = Loop(demoplots)       
    
    def next_plot(self):
        self.pcount = self.pcount + 1
        self.canvas = self.examples[self.pcount]
        self.redraw()
    
    def prev_plot(self):
        self.pcount = self.pcount - 1
        self.canvas = self.examples[self.pcount]
        self.redraw()
        


class DemoFrame3D(tkFrame3D, DemoFrame):
    """tkFrame derived object for viewing plots derived from Plot3D."""
    def __init__(self, master, demoplots):
        tkFrame3D.__init__(self, master, canvas=demoplots[0], width=600, height=600, bg='')
        self.pcount = 0                
        self.examples = Loop(demoplots)  

    def draw_canvas(self):
        self.canvas(screenmode='normal')
        tkFrame3D.draw_canvas(self)
 
        
#------------------------------------------------------------------------------

def demo():
    """Root function for Demos."""
    root = Tkinter.Tk()
    root.protocol('WM_DELETE_WINDOW', root.destroy)
    
    info = Tkinter.Label(text="Choose one:")
    button2D = Tkinter.Button(root, text='2D Demos', command=Command(demo2D, root))
    button3D = Tkinter.Button(root, text='3D Demos', command=Command(demo3D, root))
    
    
    info.grid(row=0,column=0, columnspan=2,sticky='N')
    button2D.grid(row=1, column=0, sticky = 'E')
    button3D.grid(row=1, column=1, sticky = 'W')
    
    root.mainloop()
    

        
def demo2D(master=None):
    """GUI based 2D Plot demos."""
    import demos 
    demoplots2D = [demos.curveDemo(1), demos.contourDemo2(1), demos.contourDemo1(1), 
                   demos.colorPlot3DDemo(1), demos.AllBarsDemo(1), demos.AllScattersDemo(1)]    
                   
    root = Tkinter.Toplevel(master)
    
    mainframe = Tkinter.Frame(root, bd=2, relief="sunken")
    mainframe.pack(expand=1,fill=Tkinter.BOTH, anchor='center')
    
    demo = DemoFrame(mainframe, demoplots2D)
    demo.pack(expand=1,fill=Tkinter.BOTH, anchor='n')
    
    bframe = Tkinter.Frame(root, bd = 2, relief='raised')
    bframe.pack()
    bprev = Tkinter.Button(bframe, text="Previous", command = demo.prev_plot,
                   width = 10, height = 1)
    bprev.grid(row=1, column = 0,sticky='E')
    bnext = Tkinter.Button(bframe, text="Next", command = demo.next_plot,
                   width = 10, height = 1)
    bnext.grid(row=1,column=1,sticky='W')



def demo3D(master=None):
    """Interactive GUI based 3D Plot demos."""
    import demos 
                     
    demoplots3D = [demos.functionSurfaceDemo(1), demos.functionSurfacePlusPlane(1), 
                   demos.parametricSurfaceDemo(1), demos.surfaceDemo(1)]    

    root = Tkinter.Toplevel(master)
    
    mainframe = Tkinter.Frame(root, bd=2, relief="sunken")
    mainframe.pack(expand=1,fill=Tkinter.BOTH, anchor='center')
    
    demo = DemoFrame3D(mainframe, demoplots3D)
    demo.pack(expand=1,fill=Tkinter.BOTH, anchor='n')
    
    bframe = Tkinter.Frame(root, bd = 2, relief='raised')
    bframe.pack()
    bprev = Tkinter.Button(bframe, text="Previous", command = demo.prev_plot,
                   width = 12, height = 1)
    bnext = Tkinter.Button(bframe, text="Next", command = demo.next_plot,
                   width = 12, height = 1)
    bzin = Tkinter.Button(bframe, text="Zoom In", command = demo.zoom_in,
                   width = 12, height = 1)
    bzout = Tkinter.Button(bframe, text="Zoom Out", command = demo.zoom_out,
                   width = 12, height = 1)
    bleft = Tkinter.Button(bframe, text="Rot Lt", command = demo.rot_left,
                   width = 12, height = 1)
    bright = Tkinter.Button(bframe, text="Rot Rt", command = demo.rot_right,
                   width = 12, height = 1)
    bup = Tkinter.Button(bframe, text="Rot Up", command = demo.rot_up,
                   width = 12, height = 1)
    bdown = Tkinter.Button(bframe, text="Rot Down", command = demo.rot_down,
                   width = 12, height = 1)
    breset = Tkinter.Button(bframe, text="Reset", command = demo.reset_view,
                   width = 12, height = 1)                                                                                                                                       
                   
    bprev.grid(row=1, column = 0,sticky='E')
    bup.grid(row=1, column = 1, sticky='N')                   
    bnext.grid(row=1,column=2,sticky='W')
    
    bleft.grid(row=2, column = 0,sticky='E')
    breset.grid(row=2, column = 1, sticky='N')                   
    bright.grid(row=2,column=2,sticky='W')
    
    bzin.grid(row=3, column = 0,sticky='E')
    bdown.grid(row=3, column = 1, sticky='N')                   
    bzout.grid(row=3,column=2,sticky='W')    


    
#------------------------------------------------------------------------------

if __name__ == '__main__':
    demo()
    
    
    
#------------------------------------------------------------------------------
##  $Log: tkdisipyl.py,v $
##  Revision 1.1.1.1  2002/04/11 15:19:08  pmagwene
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
##  Revision 1.5  2001/04/20 02:53:53  pmagwene
##  minor tweaks
##
##  Revision 1.4  2001/04/17 20:05:09  pmagwene
##  *** empty log message ***
##
##  Revision 1.3  2001/03/27 03:00:01  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.2  2001/03/26 01:03:23  pmagwene
##  Minor fixes
##
