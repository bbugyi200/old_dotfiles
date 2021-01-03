#!/usr/bin/python


import sys, string
from wxPython.wx import *
from disipyl import pxdislin, plots


#---------------------------------------------------------------------------

class DisipylPanel(wxPanel):
    """A Panel object modified for drawing disipyl Canvas objects.

    Comments:
        * Will also except a descendant of PlotObject and instantiate a
          disipyl Canvas object as needed.
    """
    def __init__(self, parent, canvas, id=-1, size=wxSize(500,500)):
        wxPanel.__init__(self, parent, id, size=size, style=wxSIMPLE_BORDER)

        if isinstance(canvas, pxdislin.Canvas):
            self.canvas = canvas
        elif isinstance(canvas, pxdislin.PlotObject):
            self.canvas = pxdislin.Canvas(canvas)
            self.canvas.plot.axes(centered = 1)
        else:
            raise DislinException('Non-Canvas or Plot object passed to wxdisipyl.DisipylFrame')

        EVT_PAINT(self, self.OnPaint)

        self.get_init_values()


    def get_init_values(self):
        self._origid = self.canvas.windowID
        self._origtype = self.canvas.windowtype

    def redraw(self, event):
        if self.canvas.windowID:
            self.canvas.draw()

    def OnPaint(self,event):
        # This call to wxPaintDC MUST be here!
        dc = wxPaintDC(self)
        handle = self.GetHandle()
        if handle:
            self.canvas(windowID = handle, windowtype = 'window')
            self.canvas.draw()

#---------------------------------------------------------------------------

class DisipylFrame(wxFrame):
    def __init__(self, parent, canvas, id=-1, title="DisipylFrame", size=wxSize(500,500)):
        wxFrame.__init__(self, parent, id, title, size=size)
        self._canvas = canvas
        self.canvas = pxdislin.Canvas(canvas)
        self.size = size

        # wire the event to callbacks
        EVT_CLOSE(self, self.OnExit)

        self._build_widgets()

    def _build_widgets(self):
        self.panel = DisipylPanel(self, self._canvas)

        self._pack()


    def _pack(self):
        """Pack the widgets in the frame"""
        box = wxBoxSizer(wxHORIZONTAL)
        box.Add(self.panel)

        self.SetAutoLayout(true)
        self.SetSizer(box)

    def OnExit(self, event):
        self.Destroy()
        sys.exit()


#---------------------------------------------------------------------------
# Demo
#   - Thanks to Les Schaffer who created this great example for the
#       predecessor to disipyl (pxDislin).  This demo simultaneously
#       shows the plot on the left portion of the window and the
#       code used to generat it on the right

from disipyl.demos import demolist
from disipyl.utilities import Loop

ID_NEXT, ID_PREV, ID_EXIT = (wxNewId(), wxNewId(), wxNewId())

class DemoFrame(wxFrame):
    FRAME_SZ = wxSize(950,500)
    PLOT_SZ = wxSize(500,500)
    BUTTON_SZ = wxSize(100,25)
    TEXT_SZ = wxSize(450,440)

    def __init__(self, parent, id, title):
        wxFrame.__init__(self, parent, id, title, size=self.FRAME_SZ)

        # make widgets
        self._build_widgets()

        # wire the events to callbacks
        EVT_PAINT(self, self.OnPaint)
        EVT_BUTTON(self, ID_NEXT, self.next_plot)
        EVT_BUTTON(self, ID_PREV, self.prev_plot)
        EVT_BUTTON(self, ID_EXIT, self.OnExit)
        EVT_CLOSE(self, self.OnExit)

        # start the engines
        self._init_plots()


    def _build_widgets(self):
        # plot panel: its handle gets fed to pxDislin.
        self.plotPanel = wxPanel(self, -1,
                                 size=self.PLOT_SZ,
                                 style=wxSIMPLE_BORDER)
        self.plotHandle = self.plotPanel.GetHandle()

        # prev, next, exit buttons
        self.nextB = wxButton(self, ID_NEXT, 'Next', size= self.BUTTON_SZ)
        self.prevB = wxButton(self, ID_PREV, 'Prev', size= self.BUTTON_SZ)
        self.exitB = wxButton(self, ID_EXIT, 'Exit', size= self.BUTTON_SZ)

        # text control to display code.
        self.textCtrl = wxTextCtrl(self, -1,
                                   value = self._get_text(),
                                   style=wxTE_MULTILINE|wxTE_READONLY,
                                   size = self.TEXT_SZ)

        # pack the widgets
        self._pack()

    def _pack(self):
        """Pack the widgets in the frame."""
        # buttons
        hbox = wxBoxSizer(wxHORIZONTAL)
        butL = (self.prevB, self.nextB, self.exitB)
        for b in butL:
            hbox.Add(b, 0, wxALL, 5)

        # text over buttons
        vbox = wxBoxSizer(wxVERTICAL)
        vbox.Add(self.textCtrl)
        vbox.Add(hbox, 0, wxALIGN_CENTER)

        # plot left, buttons/text right
        box = wxBoxSizer(wxHORIZONTAL)
        box.Add(self.plotPanel)
        box.Add(vbox)

        self.SetAutoLayout(true)
        self.SetSizer(box)


    def _init_plots(self):
        """Instantiate the example plots, hand'em the requisite window
        handle."""
        demos = []
        for demo in demolist:
            # this makes each demo function hand back a canvas
            demos.append(demo(1))

        # make lists of plots and respective functions
        self.plotL = Loop(demos)
        self.codeL = Loop(demolist)

        # start at the beginning
        self.pcount = 0


    def OnExit(self, event):
        self.Destroy()
        sys.exit()


    def OnPaint(self, event):
        dc = wxPaintDC(self)
        self.plotHandle = self.plotPanel.GetHandle()
        if self.plotHandle:
            # get current canvas and set it's handle
            canvas = self.plotL[self.pcount]
            canvas(windowID = self.plotHandle, windowtype = 'window')
            # draw it
            canvas.draw()
        # scroll to example code
        self.textCtrl.ShowPosition( self._pos_for_example( ) )


    # next and prev invoke OnPaint via Refresh
    def next_plot(self, event):
        self.pcount = self.pcount + 1
        self.Refresh()


    def prev_plot(self, event):
        self.pcount = self.pcount - 1
        self.Refresh()


    def _get_text(self):
        """get the code for the example currently displayed."""
        fp = open('demos.py',   'r')
        self.lines = fp.readlines()
        txt = string.join( self.lines, '')
        fp.close()
        return txt


    def _pos_for_example(self):
        """return the position in the text control of the code for the
        current example."""
        srchStr = 'def %s('%self.codeL[self.pcount].func_name
        i = 0
        while i< len(self.lines):
            if self.lines[i].find( srchStr ) >= 0:
                line = i
                break
            i += 1
        return self.textCtrl.XYToPosition(0,line)


class Demo(wxApp):
    def OnInit(self):
        self.frame = DemoFrame(NULL, -1, 'disipyl demo with wxPython Widgets')
        self.frame.Show(true)
        self.SetTopWindow(self.frame)
        return true

if __name__ == '__main__':
    app = Demo(0)
    app.MainLoop()
