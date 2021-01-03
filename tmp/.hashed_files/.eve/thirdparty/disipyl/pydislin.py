"""A light wrapper around the dislin library

An attempt to make the DISLIN library more pythonic and readable.

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
##  $Id: pydislin.py,v 1.6 2002/04/23 18:18:30 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.6 $"
__credits__ = ""

#------------------------------------------------------------------------------
import types, UserList

#------------------------------------------------------------------------------
withjython = 0
try:
    import Dislin       # try the java class library first
    dislin = Dislin
    withjython = 1
except:
    import dislin
#------------------------------------------------------------------------------
# Global options dictionaries

vectordict = {None:-1,'normal':1311,'normal fill':1301,'tiny':1111,'fork':1421}

symboldict = {None:-1,'square':0, 'octagon':1, 'triangle':2, 'cross':3,
                'x':4,'diamond':5, 'rtriangle':6, 'xbox':7, 'star':8,
                'plus diamond':9, 'plus octagon':10, 'dbl triangle':11,
                'plus square':12, 'xoctagon':13, 'sqr triangle':14,
                'circle':15,'fill square':16, 'fill octagon':17,
                'fill triangle':18,'fill diamond':19, 'fill rtriangle':20,
                'fill circle':21}

linedict = {None:-1,'solid':0, 'dot':1, 'dash':2, 'chain dash':3, 'chain dot':4,
             'medium dash': 5, 'large dot':6, 'large dash':7}

justdict = {'left':'LEFT','right':'RIGHT','center':'CENT'}

fontdict = {None:-1, 'default':dislin.disalf, 'simple':dislin.simplx,
        'complex':dislin.complx, 'double':dislin.duplx, 'triple':dislin.triplx}

shadingdict = {None:-1,'none':0, 'solid':16, 'diagonal1':1, 'diagonal2':2,
        'diagonal3':3,'diagonal4':4,'diagonal5':8, 'diagonal6':12, 'kilt':13,
        'hatch1': 5, 'hatch2':7, 'hatch3':10, 'hatch4':14, 'dash':17}

bardict = {None:-1,'vertical':'VERT', 'horizontal':'HORI', 'vertical3D':'3DVERT',
            'horizontal3D':'3DHORI'}


#------------------------------------------------------------------------------
# Level 0 Functions

def initialize():
    """Initialize DISLIN drawing. Set to DISLIN level 1."""
    dislin.disini()


def terminate():
    """Terminate DISLIN drawing. Set to DISLIN level 0."""
    dislin.disfin()


def setup():
    """Convenience function for setting up screen for interactive use."""
    setFormat()
    setFilename()
    setScreenMode()

#------------------------------------------------------------------------------
# Page Settings

def setFormat(format='screen'):
    """The DISLIN output format.

    Specifies the filetype or screen type on which the plot will be drawn.

    Valid: cgm, color postscript, console, png, postscript, pdf, screen,
                   tiff, virtual, window1, wmf
    """
    formatdict = {'screen':'XWIN', 'postscript':'POST', 'tiff':'TIFF',
            'color postscript':'PSCL', 'cgm':'CGM','window1':'XWI1',
            'wmf':'WMF', 'png':'PNG','console':'CONS', 'pdf':'PDF', 'virtual':'VIRT'}
    dislin.metafl(formatdict[format])



def setFilename(filename='dis.out',overwrite=0):
    """Set filename used for output.

    Arguments:
        * filename: name of file.
        * overwrite: whether files with same filename should be overwritten.
    """
    if overwrite:
        dislin.filmod('DELETE')
    dislin.setfil(filename)



def setScreenMode(mode='normal'):
    """Set the foreground/background color for DISLIN plots.

    valid modes:
        * normal: white background with black default foreground color
        * black: black background with white default foreground color
    """
    screendict = {'normal':'REVERS', 'black':'NOREV'}
    dislin.scrmod(screendict[mode])



def setPageSize(x,y):
    """Sets the page size for DISLIN plots (in plot coordinates).

    Valid: (int, int)
    """
    dislin.page(x,y)


def setScaling(factor=1.0):
    """Sets the scaling factor for an entire DISLIN plot.

    Valid: float (0,inf]
    """
    dislin.sclfac(factor)


def setScalingMode(mode='down'):
    """Sets the method by which graphics are scaled to hardware devices.

    valid modes:
        * down: graphics larger than hardware page size are scaled down.
        * full: graphics scaled up/down depending on hardware page size.
    """
    mdict = {'down':'DOWN','full':'FULL'}
    dislin.sclmod(mode)



def setBorder():
    """Plots a border around the page."""
    dislin.pagera()


def setPageFill(ncolor):
    """Fills the entire page with a single color.

    Valid: int [0,255]
    """
    dislin.pagfll(ncolor)


def drawPageHeader(pre="",post="",corner=3,direction=0):
    """Draws a header at a corner of the page.

    Arguments:
        * pre: character string preceding the header line
        * post: character string following header line
        * corner:  1: lower left, 2: lower right, 3: upper right, 4: upper left
        * direction: 0: horizontal, 1: vertical
    """
    dislin.paghdr(pre,post,corner,direction)



def include_metafile(filename):
    """Draws a metafile on the page.  See also fillbox."""
    dislin.incfil(filename)


def getPNGBuffer():
    """Returns a string representation of a DISLIN plot drawn in PNG format.
    """
    buf, n = dislin.rbfpng(0)
    buf, n = dislin.rbfpng(n)
    return buf


def getPDFBuffer():
    """Returns a string representation of a DISLIN plot drawn in PDF format.
    """
    buf, n = dislin.pdfbuf(0)
    buf, n = dislin.pdfbuf(n)
    return buf

def setPDFMode(compressed=1, buffer=0):
    """Specifies whether PDF files are compressed/uncompressed and whehter PDF output is buffered."""
    if compressed:
        dislin.pdfmod('ON','COMPRESSION')
    else:
        dislin.pdfmod('OFF', 'COMPRESSION')

    if buffer:
        dislin.pdfmod('ON', 'BUFFER')
    else:
        dislin.pdfmod('OFF', 'BUFFER')

#------------------------------------------------------------------------------
# Axis related

def axis2D(xlow,xhigh,xfirst,xstep,ylow,yhigh,yfirst,ystep):
    """Set up 2D axis.

    Arguments:
        * xlow, xhigh: min and max values of x-axis.
        * xfirst, xstep: start/step values for ticks on x-axis.
        * ylow, yhigh, yfirst, ystep: see above for x-axis.
    """
    dislin.graf(xlow,xhigh,xfirst,xstep,\
                ylow,yhigh,yfirst,ystep)



def setAxisType(axistype='rectangular'):
    """Sets the type of axis system drawn.

    Valid:  'rectangular', 'crossed'
    """
    adict = {'rectangular':'RECT', 'crossed':'CROSS'}
    dislin.axstyp(adict[axistype])



def setAxisPagePosition(x,y):
    """Sets the plot coordinates of the lower left corner of an axis system."""
    dislin.axspos(x,y)


def setAxisPageOrigin(x,y):
    """Sets the position (in plot coordinates) of origin of crossed-axis system."""
    dislin.axsorg(x,y)


def setAxisPageSize(width,height):
    """Defines the size (in plot coordinates) of an axis system."""
    dislin.axslen(width,height)


def centerAxis():
    """Centers an axis system on the page."""
    dislin.center()


def setAxisScaling(scalingtype='linear', axes='XYZ'):
    """Determines the type of the axis system.

    valid scalingtypes: linear, log
    Arguments:
        * scalingtype:
        * axes: determines which axis to apply the operation to.
    """
    scalingdict = {'linear':'LIN', 'log':'LOG'}
    dislin.axsscl(scalingdict[scalingtype],axes)



def autoAxisScaling(vlist, axes='XYZ'):
    """Tries to determine reasonable axis scaling based on passed in values.

    Use with caution.
    """
    vlist = list(vlist[:])
    l,h = min(vlist), max(vlist)
    low = l - (h-l)/50.0
    high = h + (h-l)/50.0
    vlist.extend([low,high])
    dislin.setscl(vlist, len(vlist), axes)


def setTickNumber(n=2, axes='XYZ'):
    """Sets number of ticks between labels on axes."""
    dislin.ticks(n, axes)


def setTickPosition(position='sameaslabels', axes='XYZ'):
    """Sets position of ticks on given axis.

    Valid:
        * sameaslabels: ticks plotted on same side as labels.
        * inside: ticks plotted on inside of axis line.
        * center: ticks centered on axis line.
    """
    tickdict = {'sameaslabels':'LABELS', 'inside':'REVERS','center':'CENTER'}
    dislin.ticpos(tickdict[position],axes)



def setTickLength(major=24,minor=16):
    """Sets lengths (in plot coordinates) of major/minor ticks."""
    dislin.ticlen(major,minor)


def setLabelType(ltype='float', axes='XYZ'):
    """Sets the type of labels which will be plotted on an axis.

    See DISLIN docs on LABELS for more info.
    """
    labeldict = {'none':'NONE', 'float':'FLOAT', 'exp':'EXP', 'fexp':'FEXP',
                'log':'LOG','clog':'CLOG','elog':'ELOG', 'time':'TIME',
                'hours':'HOURS', 'secs':'SECONDS', 'date':'DATE',
                'map':'MAP', 'lmap':'LMAP', 'dmap':'DMAP', 'mylab':'MYLAB'}
    dislin.labels(labeldict[ltype], axes)


def setMyLabels(labels, axes='XYZ', step=1):
    """Set up custom labels that are used with 'mylab' label type.

    labels should be a sequence of strings.
    """
    resetParameter('mylab')
    for index in range(0, len(labels), step):
        dislin.mylab(labels[index], index/step+1, axes)


def setLabelOrientation(orientation='horizontal',axes='XYZ'):
    """Defines the orientation of axis labels.

    valid orientations: horizontal, vertical
    """
    orientdict = {'horizontal':'HORI', 'vertical':'VERT'}
    dislin.labtyp(orientdict[orientation],axes)



def setLabelPosition(position='ticks', axes='XYZ'):
    """Defines how axis labels are drawn relative to ticks.

    valid positions: ticks, center, shift
        * ticks: labels drawn at major ticks
        * center: labels drawn between major ticks
        * shift: starting/ending labels shifted
    """
    pdict = {'ticks':'TICKS', 'center':'CENTER', 'shift':'SHIFT'}
    dislin.labpos(pdict[position], axes)



def setLabelJustification(justify='auto', axes='XYZ'):
    """Defines justification of axis labels.

    valid justify: auto, left, right, outward, inward
        * auto: automatically determined by DISLIN
        * left: left-justified
        * right: right-justified
        * outward: left justified on left and lower axes, right-justified on
            right and upper axes.
        * inward: opposite of outward.
    """
    ldict = {'auto':'AUTO', 'left':'LEFT','right':'RIGHT',\
                'outward':'OUTW','inward':'INWA'}
    dislin.labjus(ldict[justify],axes)



def setLabelDigits(digits=1, axes='XYZ'):
    """Sets number of digits after the decimal point for axis labels."""
    dislin.labdig(digits, axes)


def setIntegerLabels():
    """Causes axes labels to be drawn using integer values."""
    dislin.intax()


def setLabelDistance(dist=24, axes='XYZ'):
    """Determines the distance (in plot coordinates) between axis ticks and labels."""
    dislin.labdis(dist, axes)


def suppressLabels(mode='none', axes='XYZ'):
    """Suppresses particular labels on axes.

    valid modes: none, first, nofirst, last, nolast, ends, noends
        * none: all labels displayed.
        * first: only starting label displayed
        * nofirst: starting label NOT displayed
        * ends: only ending label displayed
        * noends: ending label NOT displayed
    """
    mdict = {'none':'NONE', 'first':'FIRST','last':'LAST',
         'nofirst':'NOFIRST','nolast':'NOLAST', 'ends':'ENDS','noends':'NOENDS'}
    dislin.axends(mdict[mode], axes)



def setAxisName(name, axes='XYZ'):
    """Sets the name (title) drawn next to an axis."""
    dislin.name(name, axes)


def setAxisNameHeight(h):
    """Sets the height (in plot coordinates) of axis names."""
    dislin.hname(h)


def setAxisNameDistance(dist,axes='XYZ'):
    """Sets the distance (in plot coordinates) between axis labels and axis names."""
    dislin.namdis(dist, axes)


def setAxisNameJustification(jus, axes='XYZ'):
    """Sets justification of axis names."""
    dislin.namjus(justdict[jus],axes)


def noAxisLines(axes='XYZ'):
    """Suppresses the plotting of axis lines.
    """
    dislin.frame(0)
    dislin.noline(axes)


def noAxisSystem():
    """Suppresses the plotting of an axis system."""
    dislin.nograf()


def setAxisParts(lowx='all', lefty='all', upx='ticks', righty='ticks'):
    """Determines the parts of an axis system which will be drawn.

    valid arguments: none, lines, ticks, labels, all
        * none: complete axes suppressed
        * lines: only axis lines drawn
        * ticks: lines and ticks drawn
        * labels: lines, ticks, and labels drawn
        * all: lines, ticks, labels, and axis names drawn
    """
    partdict = {'none':'NONE','lines':'LINE','ticks':'TICKS',
                'labels':'LABELS', 'all':'NAME'}
    dislin.setgrf(partdict[lowx], partdict[lefty],\
                  partdict[upx], partdict[righty])



def setFrameThickness(n=1):
    """Determines thickness of frame drawn around axis systems."""
    dislin.frame(n)


def setAxisBackground(idx=-1):
    """Sets the color(index) of the background for axis systems."""
    dislin.axsbgd(idx)


def setAxisColors(line=-1,tick=-1,label=-1,name=-1, axes='XYZ'):
    """Sets color for indicated elements of an axis."""
    setAxisLineColor(line, axes)
    setAxisTickColor(tick, axes)
    setAxisLabelColor(label, axes)
    setAxisNameColor(name, axes)

def setAxisAllColor(idx=-1, axes='XYZ'):
    """Sets the color(index) for all axis parts."""
    dislin.axclrs(idx, 'ALL', axes)


def setAxisLineColor(idx=-1, axes='XYZ'):
    """Sets the color(index) for axis lines."""
    dislin.axclrs(idx, 'LINE', axes)


def setAxisTickColor(idx=-1, axes='XYZ'):
    """Sets the color(index) for axis ticks."""
    dislin.axclrs(idx, 'TICKS', axes)


def setAxisLabelColor(idx=-1, axes='XYZ'):
    """Sets the color(index) for axis labels."""
    dislin.axclrs(idx, 'LABELS', axes)


def setAxisNameColor(idx=-1, axes='XYZ'):
    """Sets the color(index) for axis names."""
    dislin.axclrs(idx, 'Name', axes)


def end_axis():
    """Terminate axis system."""
    dislin.endgrf()


def setTitle(title, height=48, justify='center', pos='above', offset=0):
    """Draws title for an axis system.

    Arguments:
        * title: text of title (can contain line ends)
        * height: height of title text
        * justify: justification of title text
        * pos: position of title text relative to axis system (above, below)
        * offset: spacing of title relative to axis system.  Negative values
            move title closer to axis system.  Positive values move title
            further away from axis system.
    """
    titles = title.split("\n")[:4]
    i = 1
    for line in titles:
        dislin.titlin(line, i)
        i = i + 1
    dislin.titjus(justdict[justify])
    pdict = {'above':'ABOVE', 'below':'BELOW'}
    dislin.titpos(pdict[pos])
    dislin.htitle(height)
    dislin.vkytit(offset)
    dislin.title()



def setGrid(nx,ny,gridtype='square'):
    """Sets grid (of gridtype) with spacing given by nx, ny.

    valid gridtypes: square, polar
    """
    if gridtype == 'polar':
        dislin.grdpol(nx,ny)
    else:
        dislin.grid(nx,ny)


def setSquareGrid(nx,ny):
    """Overlay grid on axis system, with nx and ny grid lines between labels."""
    dislin.grid(nx,ny)


def setPolarGrid(nc,nl):
    """Overlay polar grid on axis system, with nc circles and nl sector lines."""
    dislin.grdpol(nc,nl)


def setOriginLines(val="xy"):
    """Draws vertical and horizontal lines through the origin of axis system.

    Specify: "x" -- for y=0 only
             "y" -- for x=0 only
             "xy" -- for x=0 and y=0
             "cross" -- for x=0,y=0 plus ticks
    """
    if val == "x":
        dislin.xaxgit()
    elif val == "y":
        dislin.yaxgit()
    elif val == "cross":
        dislin.cross()
    else:
        dislin.axgit()



def secondaryXaxis(low,high,first,step,length,name,direction,x,y,log=0):
    """Plots a secondary X axis. See XAXIS in DISLIN docs."""
    if log:
        dislin.xaxlg(low,high,first,step,length,name,direction,x,y)
    else:
        dislin.xaxis(low,high,first,step,length,name,direction,x,y)


def secondaryYaxis(low,high,first,step,length,name,direction,x,y,log=0):
    """Plots a secondary Y axis. See YAXIS in DISLIN docs."""
    if log:
        dislin.yaxlg(low,high,first,step,length,name,direction,x,y)
    else:
        dislin.yaxis(low,high,first,step,length,name,direction,x,y)


def disableAxisClipping():
    """Disables clipping of objects outside axes(and pages?)"""
    dislin.noclip()

#------------------------------------------------------------------------------
# Colors

def setColor(clr):
    """Sets the color used to draw DISLIN object.

    Works with strings (basic color name), integers (color index), and 3-tuples (RGB).

    Valid:
        Strings: 'black', 'red', 'green', 'blue', 'cyan', 'yellow', 'orange',
                    'magenta', 'white', 'fore'
        Integers: int [0,255]
        Tuple: (int r,int g, int b)
    """
    if type(clr) == types.StringType:
        setColorString(clr)
        return
    if type(clr) == types.IntType:
        setColorIndex(clr)
        return
    if type(clr) == types.TupleType:
        setColorRGB(*clr)


def setColorString(clr):
    """Sets color using simple string"""
    dislin.color(clr)


def setColorIndex(idx):
    """Sets color using integer in range 0 to 255"""
    dislin.setclr(idx)


def setColorRGB(r,g,b):
    """Sets color using RGB values

   Dislin uses values between (0,1). I assume you'll want to use standard
   values in range (0,255) so I do an automatic conversion.

   Dislin only supports 256 colors
    """
    r, g, b = r/255., g/255., b/255.
    dislin.setrgb(r,g,b)



def setColorTable(table='rainbow'):
    """Sets a standard DISLIN supplied color table.

    valid tables: small, vga, grescale, rainbow, violet, reverse grey,
                  reverse rainbow, reverse violet
    """
    colortabledict = {'small':'SMALL','vga':'VGA','rainbow':'RAIN',
            'violet':'SPEC', 'greyscale':'GREY', 'reverse rainbow':'RRAIN',
            'reverse violet':'RSPEC', 'reverse grey':'RGREY'}
    dislin.setvlt(colortabledict[table])



def setMyColorTable(lr,lg,lb):
    """Defines a custom color table."""
    dislin.myvlt(lr,lg,lb,len(lr))


def expandColorBarScaling(direction='none'):
    """Expands the numbering of log scaled Z-axis to next order of magnitude up/down.

    From DISLIN docs:
    The routine EXPZLB expands the numbering of a logarithmically scaled Z-axis
    to the next order of magnitude that lies up or down the axis limits.
    The scaling of the colour bar will not be changed. This routine is useful
    if the range of the Z-axis scaling is smaller than 1 order of magnitude.
    """
    ddict = {'none':'NONE','down':'FIRST','both':'BOTH'}
    dislin.expzlb(ddict[direction])

#------------------------------------------------------------------------------
# Text and Numbers

def setTextHeight(h=36):
    """Sets the character height of text."""
    dislin.height(h)


def setTextAngle(deg=0):
    """Sets the angle (in degrees) at which text is drawn."""
    dislin.angle(deg)


def setTextJustification(justify='left'):
    """Sets the justification of text."""
    dislin.txtjus(justdict[justify])


def isPostscript(fmt):
    """Returns true if passed format is a postscript or PDF format."""
    if fmt == 'POST' or fmt == 'PSCL' or fmt == 'PDF':
        return 1
    return 0

def isScreen(fmt):
    """Returns true if passed format is a screen or console format."""
    if fmt == 'CONS' or fmt == 'XWIN' or fmt =='XWLi':
        return 1
    return 0

def isWMF(fmt):
    """Returns true if format is a windows metafile format."""
    if fmt == 'WMF':
        return 1
    return 0

def setCharacterCode(code='STANDARD'):
    """Sets the character coding for text."""
    dislin.chacod(code)

def setFont(font='default',hardware=1):
    """Sets a font depending on current DISLIN file format."""
    if font == 'default' and hardware:
        setHardwareFont()
        return
    currfmt = getFileFormat()
    if isPostscript(currfmt):
        setPSFont(font)
    elif isWMF(currfmt):
        setTTFont(font)
    else:
        setDislinFont(font)

def setHardwareFont():
    """Sets sensible default fonts depending on the file type."""
    dislin.hwfont()


def setDislinFont(font='default'):
    """Sets font type to DISLIN build in font."""
    fontdict[font]()


def setPSFont(font='default'):
    """Sets font type to postscript fonts."""
    if font == 'default':
        font = 'Times-Roman'
    dislin.psfont(font)


def setTTFont(font='default'):
    """Sets font type to true type fonts."""
    if font == 'default':
        font = 'Times New Roman'
    dislin.winfnt(font)


def setX11Font(font='default', ending='STANDARD'):
    """Sets font type to X11 fonts."""
    if font == 'default':
        font = '-Adobe-Times-Bold-R-Normal-'
    dislin.x11fnt(font,ending)


def drawText(txt,x,y,ucoords=1,TeX=0):
    """Draw text message at x,y. Uses TeX fonts if TeX is true."""
    if (type(txt) is types.IntType) or (type(txt) is types.FloatType) or \
    (type(txt) is types.LongType):
        drawNumber(txt,x,y,ucoords)
    else:
        if TeX: setTeXMode(1)
        drawMessage(txt,x,y,ucoords)


def drawMessage(txt,x,y,ucoords=1):
    """Draws a text message at (x,y).

    Arguments:
        * txt: the text message to draw
        * x,y: coordinates at which text is drawn
        * ucoords: if true then draw, using user coordinates, else drawn
            using page coordinates
    """
    if ucoords:
        dislin.rlmess(txt,x,y)
    else:
        dislin.messag(txt,x,y)



def setTeXMode(mode):
    """Sets TeXMode for font drawing."""
    if mode:
        dislin.texmod('ON')
    else:
        dislin.texmod('OFF')


def drawNumber(n,ndig,x,y, ucoords=1, fmt='float'):
    """Draws a number at (x,y).

    Arguments:
        * n: the number to draw
        * ndig: number of digits following decimal point (-1 = Integer)
        * ucoords: if true then draw, using user coordinates, else drawn
            using page coordinates
    """
    fdict = {'float':'FLOAT','exp':'EXP','fexp':'FEXP','log':'LOG'}
    dislin.numfmt(fdict[fmt])
    if ucoords:
        dislin.rlnumb(n,ndig,x,y)
    else:
        dislin.number(n,ndig,x,y)


#------------------------------------------------------------------------------
# Symbols

def drawSymbol(x,y,nsymb='circle',ucoords=1):
    """Draws a symbol at (x,y), either page or user coords."""
    if ucoords:
        dislin.rlsymb(symboldict[nsymb],x,y)
    else:
        dislin.symbol(symboldict[nsymb],x,y)


def setSymbolSize(x=30):
    """Sets symbol size (in plot coordinates)."""
    dislin.hsymbl(x)


def setSymbolRotation(deg=0):
    """Sets rotation angle of symbol (in degrees)."""
    dislin.symrot(deg)


#------------------------------------------------------------------------------
# Lines, Vectors,  Curves, Error Bars, Vector fields

def drawLine(x0,y0,x1,y1,ucoords=1):
    """Draws a Line from (x0,y0) to (x1,y1), either page or user coords."""
    if ucoords:
        dislin.rline(x0,y0,x1,y1)
    else:
        dislin.line(x0,y0,x1,y1)


def calcArrowHead(ratio=1, size=3, form='empty', position='end'):
    """Calculates arrow head code for vectors.

    Arguments:
        - ratio: ratio of width to length (1..9)
        - size: size of arrowhead (1..9)
        - form: form of arrowhead ('filled', 'empty', 'open', 'closed')
        - position: where arrow heads will be drawn ('none', 'end',
            'opposite', 'same')
            -- 'none' means no arrowheads drawn
            -- 'end' means arrow heads draw at end points only
            -- 'opposite' means arrowheads drawn at start and end points
                with arrowheads pointing in opposite direction
            -- 'same' means arrowheads drawn at start and end points
                with arrowhead pointing in same direction
    """
    fdict = {'filled':'0', 'empty':'1', 'open':'2', 'closed':'3'}
    pdict = {'none':'0', 'end':'1', 'opposite':'2', 'same':'3'}
    val = str(ratio)+str(size)+fdict[form]+pdict[position]
    return int(val)



def drawVector(x0,y0,x1,y1,vtype='normal fill', ucoords=1):
    """Draws a vector from (x0,y0) to (x1,y1), either page or user coords."""
    ivec = vectordict[vtype]
    if ucoords:
        dislin.rlvec(x0,y0,x1,y1,ivec)
    else:
        dislin.vector(x0,y0,x1,y1,ivec)


def drawVectorField(x0list,y0list,x1list,y1list,vtype='normal'):
    """Draws a vector field based on list of starting/ending x,y values."""
    ivec = vectordict[vtype]
    dislin.field(x0list,y0list,x1list,y1list,len(x0list),ivec)


def setLineWidth(w=1):
    """Sets width of DISLIN lines."""
    dislin.linwid(w)


def setLineStyle(style='solid'):
    """Sets style with which lines are drawn.

    valid styles: solid, dot, dash, chain dash, chain dot, medium dash,
                  large dot, large dash
    """
    dislin.lintyp(linedict[style])


def drawCurve(xlist,ylist):
    """Draws curve which goes through points specified by xlist,ylist."""
    dislin.curve(xlist,ylist,len(xlist))


def setCurveInterpolation(mode):
    """Sets interpolation mode for curve drawing.

    Valid: linear, step, stairs, bars, stem, spline, pspline

    See DISLIN docs for POLCRV for more info.
    """
    curvedict = {'linear':'LINEAR', 'step':'STEP', 'stairs':'STAIRS',
            'bars':'BARS', 'stem':'STEM', 'spline':'SPLINE',
            'pspline':'PSPLINE'}
    dislin.polcrv(curvedict[mode])


def setSplineMode(order=3,npts=200):
    """Defines the order of polynomial and number of points for splined curves."""
    dislin.splmod(order,npts)


def setCurveWidth(w):
    """Sets the thickness of DISLIN curves."""
    dislin.thkcrv(w)


def setBarWidth(w):
    """Sets the thicknes of bars plotted by curves."""
    dislin.barwth(w)


def setCurveSymbols(step=0,shape='circle',size=30):
    """Sets the symbols used to mark points on curves.

    Arguments:
        * step: if step=0, no symbols are drawn, only lines.  If step is negative,
            only every n-th symbols is drawn, without lines.  If step is positive
            lines are drawn, and every n-th symbol is drawn.
        * symbolshape: the shape of the symbols to use (see symboldict)
        * size: size of the symbols
    """
    dislin.marker(symboldict[shape])
    dislin.hsymbl(size)
    dislin.incmrk(step)



def drawShadedCurve(x0list,y0list,x1list,y1list):
    """Draws a shaded region between two curves."""
    dislin.shdcrv(x0list,y0list,len(x0list),x1list,y1list,len(x1list))


def setShadingPattern(pattern,outline=1):
    """Sets the shading pattern for DISLIN objects."""
    dislin.shdpat(shadingdict[pattern])
    if not outline:
        dislin.noarln()


def drawErrorBars(xlist,ylist,elow,ehigh, symbol='square', symbolsize=30,horizontal=0):
    """Draws error bars.

    Arguments:
        * xlist, ylist: the x,y values of center of each error bar
        * elow, ehigh: the high/low values of the errors at each point
        * symbol: the symbol used to indicate center of error bar
        * symbolsize: size of symbol drawn at center of error bar
        * horizontal: whether error bars should be drawn horizontal rather than
            vertical
    """
    if symb:
        dislin.incmrk(1)
        dislin.marker(symboldict[symbol])
        dislin.hsymbl(symbolsize)
    else:
        dislin.incmrk(0)
    if horizontal:
        dislin.bartyp('HORI')
    dislin.errbar(xlist,ylist,elow,ehigh,len(xlist))


#------------------------------------------------------------------------------
# Geometric Figures

def drawRectangle(x,y,width,height,rounding=0,ucoords=1):
    """Draw rectangle with upper left corner at x,y.

    Arguments:
        * x,y: coordinates of upper left corner
        * width, height: width and height of rectangle
        * rounding: the amount of rounding of corners (0...9)
        * ucoords: whether user or page coords should be used
    """
    if ucoords:
        dislin.rlrnd(x,y,width,height,rounding)
    else:
        dislin.rndrec(x,y,width,height,rounding)



def drawCircle(x,y,radius,ucoords=1):
    """Draws circle at x,y with given radius."""
    if ucoords:
        dislin.rlcirc(x,y,radius)
    else:
        dislin.circle(x,y,radius)


def drawEllipse(x,y,a,b,ucoords=1):
    """Draws ellipse at x,y with given radii (a,b)."""
    if ucoords:
        dislin.rlell(x,y,a,b)
    else:
        dislin.ellips(x,y,a,b)


def drawPie(x,y,radius,alpha,beta,ucoords=1):
    """Draw pie slice at x,y with given radius and angles.

    Arguments:
        * x,y: coordinates of center
        * radius: radius of pie
        * alpha, beta: starting/ending angles (degrees, counter-clockwise)
    """
    if ucoords:
        dislin.rlpie(x,y,radius,alpha,beta)
    else:
        dislin.pie(x,y,radius,alpha,beta)



def drawEllipticArc(x,y,a,b,alpha,beta,theta,ucoords=1):
    """Draw elliptical arc centered at x,y.

    Arguments:
        * x,y: coordinates of center
        * a,b: radii of major/minor axes
        * alpha, beta: starting/ending angles (degrees, counter-clockwise)
        * theta: angle of rotation of whole figure (degrees, counter-clockwise)
    """
    if ucoords:
        dislin.rlarc(x,y,a,b,alpha,beta,theta)
    else:
        dislin.arcell(x,y,a,b,alpha,beta,theta)



def drawPolygon(xlist,ylist,ucoords=1):
    """Draw polygon with coordinates given by xlist, ylist."""
    if ucoords:
        dislin.rlarea(x,y,len(xlist))
    else:
        dislin.areaf(x,y,len(ylist))


#------------------------------------------------------------------------------
# Bar Chart Functions

def drawBar(xlist, ylist0, ylist1):
    """Creates bars for bar graphs.

    Arguments:
        * xlist: the positions along x-axis for drawing bars
        * ylist0, ylist1: start/end points along y-axis
    """
    dislin.bars(xlist,ylist0,ylist1,len(xlist))



def setBarType(bartype='vertical'):
    """Sets direction and appearance of bars.

    valid bartypes: vertical, horizontal, vertical3D, horizontal3D
    """
    dislin.bartyp(bardict[bartype])



def setBarWidth(w=0.75):
    """Sets width of bars for bar graphs."""
    dislin.barwth(w)


def setBarWidthMode(mode='fixed'):
    """Set width mode for bar graphs.

    valid modes: fixed, variable
    """
    bdict = {'fixed':'FIXED','variable':'VARIABLE'}
    dislin.barmod(bdict[mode], 'WIDTH')



def setBarPosition(pos='none'):
    """Defines how bars will be drawn relative to axes/ticks.

    valid positions: none, ticks, axis, both
        * none: positions defined by drawBar
        * ticks: centered on major ticks
        * axis: vertical bars start at x-axis, horizontal at y-axis
        * both: combines ticks and axis
    """
    bdict = {'none':'NONE','ticks':'TICKS','axis':'AXIS','both':'BOTH'}
    dislin.barpos(bdict[pos])



def setBarGroups(ngroups, gap):
    """Puts bars with same axis position into groups."""
    dislin.bargrp(ngroups, gap)


def setBarColor(front=-1,side=-1,top=-1):
    """Sets colors(index) for front, side, and top of bars. -1 indicates current color."""
    dislin.barclr(front,side,top)


def setBarBorderColor(clr=-1):
    """Set color(index) of bar borders. -1 indicates current color."""
    dislin.barbor(clr)


def setBar3DLook(depth=-1, ang=45):
    """Sets appearance of 3D bars.

    Arguments:
        * depth: floating point number defining depth of bar
        * ang: angle (in degrees) between front and side panels
    """
    dislin.baropt(depth,ang)


def setBarLabelStyle(mode='none',position='auto', digits=1,color=-1):
    """Sets various label options for bar graphs.

    Arguments:
        * mode : defines what kind of labels will be used
            - valid modes: none, second, first, delta -- see DISLIN docs.
        * position : defines position of labels
            - valid positions: inside, outside, left, right, center, auto
        * digits : number of digits used in labels
        * color : label color (index)
    """
    mdict = {'none':'NONE','second':'SECOND','first':'FIRST','delta':'DELTA'}
    pdict = {'inside':'INSIDE','outside':'OUTSIDE','left':'LEFT','right':'RIGHT',
             'center':'CENTER','auto':'AUTO'}
    dislin.labels(mdict[mode],'BARS')
    dislin.labpos(pdict[position],'BARS')
    dislin.labdig(digits,'BARS')
    dislin.labclr(color,'BARS')


#------------------------------------------------------------------------------
# Pie Chart Functions

def drawPieChart(xlist, labels=[]):
    """Draws a Pie chart with slice sizes defined by values of xlist.

    See DISLIN docs on PIEGRF function.
    """
    nlines = 0
    if len(labels):
        maxlen = max([len(i) for i in labels])
        #maxlines = max([len(i.split("\n")) for i in labels])
        nlines  = len(labels[0].split("\n"))
        initializeLegend(len(labels)*nlines, maxlen)
        i = 1
        for label in labels:
            for line in label.split("\n"):
                addLegendLine(line, i)
                i = i + 1
    dislin.piegrf(' ', nlines, xlist, len(xlist))



def setPieAppearance(cycle='pattern', ptype='2D'):
    """Defines how Pie colors/patterns change, and whether to use 2D/3D look."""
    cdict = {'none':'NONE','color':'COLOR','pattern':'PATTERN','both':'BOTH'}
    dislin.pietyp(ptype)
    dislin.chnpie(cdict[cycle])


def setPieLabelStyle(mode='percent',position='internal',justify='center', digits=1, color=-1):
    """Sets label style for Pie slice labels.

    valid modes: none, percent, data, both (see DISLIN docs)
    valid positions: internal, external, aligned (see DISLIN docs)
    valids justify: center, left, right, outwards, inwards (see DISLIN docs)
    """
    mdict = {'none':'NONE','percent':'PERCENT','data':'DATA','both':'BOTH'}
    pdict = {'internal':'INTERNAL','external':'EXTERNAL','aligned':'ALIGNED'}
    jdict = {'center':'CENTER','left':'LEFT','right':'RIGHT',
             'outwards':'OUTWARDS','inwards':'INWARDS'}
    dislin.labels(mdict[mode],'PIE')
    dislin.labpos(pdict[position],'PIE')
    dislin.labtyp(jdict[justify],'PIE')
    dislin.labdig(digits,'PIE')
    dislin.labclr(color,'PIE')



def setPieSliceColors(topcolors, sidecolors):
    """Sets colors(index) of top/side of pie slices."""
    dislin.pieclr(topcolors, sidecolors, len(topcolors))


def setPieBorderColor(colorindex):
    """Set the color(index) of the borders on pie graphs."""
    dislin.piebor(colorindex)


def setPieOptions3D(thickness=0.2, angle=45.0):
    """Sets thickness and viewing angle for 3D pie graphs."""
    dislin.pieopt(thickness, angle)


def setPieLabels(label, position):
    """Sets Labels drawn on left or right of data values in pie graphs

    valid positions: left, right
    """
    pdict = {'left':'LEFT', 'right':'RIGHT'}
    dislin.pielab(label, pdict[position])


def setPieAutoOffset():
    """Offsets pie slices."""
    dislin.pieexp()


def setPieArrowType(linetype='broken', head=2301):
    """Sets aspect of the arrows drawn from labels to pie slices."""
    dislin.pievec(head, linetype)


#------------------------------------------------------------------------------
# Legends

def initializeLegend(nlines, maxlen):
    """Initialize a DISLIN legend."""
    dislin.legini(' ',nlines, maxlen)


def addLegendLine(line,n):
    """Add a line to a DISLIN legend."""
    dislin.leglin(' ',line,n)


def drawLegend(pos='pageUR'):
    """Draw DISLIN legend at given position.

    valid positions: pageLL, pageLR, pageUR, pageUL, axisLL, axisLR, axisUR, axisUL
        * where page/axis refers to axis system or page system and LL, LR, UR, UL
          correspond to lower-left, lower-right, upper-right, and upper-left
    """
    pdict = {'pageLL':1, 'pageLR':2, 'pageUR':3, 'pageUL':4,
             'axisLL':5, 'axisLR':6, 'axisUR':7, 'axisUL':8}
    if type(pos) == types.TupleType:
        dislin.legpos(pos[0],pos[1])
        dislin.legend(' ', pos[0])
        return
    dislin.legend(' ',pdict[pos])



def setLegendFrameWidth(w=1):
    """Sets width of frame drawn around legend boxes."""
    dislin.frame(w)


def setLegendTitle(title):
    """Sets text of title drawn for legend."""
    dislin.legtit(title)



def setLegendPattern(nline, lwidth=1, lstyle=None, symbol=None, color=-1, pattern=None):
    """Sets aspect of the given legend line.

    Arguments:
        * nline: integer value corresponding to the legend item to be set.
        * lwidth: width of line drawn for legend item
        * lstyle: style of line drawn for legend item (see linedict)
        * symbol: shape of symbol draw for legend item (see symboldict)
        * color: color(index) for legend item
        * pattern: shading pattern for legend item
    """
    dislin.legpat(linedict[lstyle],lwidth, symboldict[symbol],
                  color, shadingdict[pattern], nline)



#------------------------------------------------------------------------------
# Misc

def clearScreen():
    """Erase the DISLIN plot."""
    dislin.erase()


def resetParameter(pname):
    """Reset given DISLIN parameter."""
    dislin.reset(pname)


def setErrorFile(fname='dis.err'):
    """Sets the name of the file to which DISLIN errors are written."""
    dislin.errfil(fname)


def setErrorDevice(device='file'):
    """Specifies whether DISLIN errors are written to the error file or screen.

    valid: file, screen
    """
    edict = {'file':'FILE','screen':'CONS'}
    dislin.errdev(edict[device])


def suppressMessages():
    """Suppresses DISLIN error messages."""
    dislin.unit(0)


def setWindowApp(w=0):
    """Makes DISLIN windows act like a console or window app with respect to errors.

    Specifies DISLIN's error behavior - acting either like a windows
    application (popping up annoying dialogs) or like a console app (writing
    the mistake to stdout).

    Valid: 0 (console) or 1 (window)
    """
    wdict = {0:'CONSOLE',1:'WINDOWS'}
    dislin.winapp(wdict[w])


def setWindowGeometry(x,y,width,height):
    """Sets the screen geometry (x,y,width,height) of DISLIN windows.

    Valid: (int, int, int, int)
    """
    dislin.window(x,y,width,height)


def setWindowSize(width,height):
    """Sets the width, height of DISLIN windows."""
    dislin.winsiz(width,height)


def setWindowKey(key='return'):
    """Determines what mouse clicks and keys enable program continuation.

    valid:
        * click: only a click of mouse button two enables continuation
        * return: return key or mouse button two enable continuation
        * escape: escape key or mouse button two enable continuation
    """
    wdict = {'click':'NONE','return':'RETURN','escape':'ESCAPE'}
    dislin.winkey(wdict[key])



def setColorMode(mode='full'):
    """Defines the color mode (low, none) for X11 output.

    Valid: 'low' or 'full'
    """
    mdict = {'low':'NONE','full':'FULL'}
    dislin.clrmod(mdict[mode])


def setWindowStore(s=0):
    """Whether X11 backing store will be used when windows are drawn.

    Valid: 0 (no store), 1 (store)
    """
    sdict = {0:'NOSTORE',1:'STORE'}
    dislin.x11mod(sdict[s])


def setWindowHold(mode='hold'):
    """Sets the windows termination handling.

    Valid:
        * hold: DISFIN waits for mouse button 2
        * nohold: no waiting for mouse button, after DISFIN all windows are closed
        * noerase: program blocked but windows not deleted after program continuation
        * none: program not blocked and windows not deleted
    """
    sdict = {'hold':'FULL','nohold':'NOHOLD','noerase':'NOERASE','none':'NONE'}
    dislin.winmod(sdict[mode])



def setWindowID(wid = 0, wtype='none'):
    """Makes DISLIN draw to window with given ID (handle)
    """
    wdict = {'none':'NONE','window':'WINDOW','pixmap':'PIXMAP','widget':'WIDGET'}
    dislin.setxid(wid, wdict[wtype])


def noCheck():
    """Suppresses listing of points outside of axis."""
    dislin.nochek()

#------------------------------------------------------------------------------
# Plot transformations -- See section on Base Transformation in DISLIN docs

def setPlotShift(x,y):
    """Shifts plot vector given amount in x,y direction."""
    dislin.trfshf(x,y)


def setPlotScaling(x,y):
    """Affects scaling of plot vectors."""
    dislin.trfscl(x,y)


def setPlotRotation(ang, x,y):
    """Affects rotation of plot vectors."""
    dislin.trfrot(ang,x,y)


def resetTransformations():
    """Resets all base transformations of plot vectors."""
    dislin.trfres()

#------------------------------------------------------------------------------
# Paramater requesting routines

def getColorIndex():
    return dislin.getclr

def getColorRGB():
    return dislin.getrgb()

def getRGBFromIndex(idx):
    return dislin.getind(idx)

def getColorBarRange():
    return getran()

def getShadingPattern():
    return dislin.getpat()

def getFileName():
    return dislin.getfil()

def getPageSize():
    return dislin.getpag()

def getAxisOrigin():
    return dislin.getor()

def getAxisPosition():
    return dislin.getpos()

def getAxisData(axis):
    return dislin.getgrf(axis)

def getAxisScaling():
    return dislin.getscl()

def getAxisLengths():
    return dislin.getlen()

def getLineWidth():
    return dislin.getlin()

def getLineStyle():
    return dislin.gettyp()

def getTextAngle():
    return dislin.getang()

def getTextHeight():
    return dislin.gethgt()

def getTerminalType():
    return dislin.getdsp()

def getScreenSize():
    return dislin.getscr()

def getWindowID():
    return dislin.getxid('WINDOW')

def getWindowGeometry():
    return dislin.getwin()

def getFileFormat():
    return dislin.getmfl()

#------------------------------------------------------------------------------
# Conversion routines

def userToPlot(x=0,y=0,z=0):
    """Converts user coordinates to plot coordinates."""
    return dislin.nxposn(x), dislin.nyposn(y), dislin.nzposn(z)

def userToPlotX(x):
    """Converts user x coordinates to plot x coordinates."""
    return dislin.nxposn(x)

def userToPlotY(y):
    """Converts user y coordinates to plot x coordinates."""
    return dislin.nyposn(y)

def userToPlotZ(z):
    """Converts user z coordinates to plot x coordinates."""
    return dislin.nzposn(z)

def plotToUser(x=0,y=0):
    """Converts plot x,y coordinates to user coordinates."""
    return dislin.xinvrs(x), dislin.yinvrs(y)



#------------------------------------------------------------------------------
# 3D axes and viewing

def axis3D(xlow,xhigh,xfirst,xstep,ylow,yhigh,yfirst,ystep,\
            zlow,zhigh,zfirst,zstep):
    """Set up 3D axis."""
    dislin.graf3d(xlow,xhigh,xfirst,xstep,ylow,yhigh,yfirst,ystep,\
                  zlow,zhigh,zfirst,zstep)


def setAxisLengths3D(x=2.,y=2.,z=2.):
    """Sets the lengths of axes in absolute 3D coordinates. See DISLIN manual."""
    dislin.axis3d(x,y,z)


def setView3D(x,y,z, viewtype='absolute'):
    """Sets position of the 3D viewpoint.

    valid viewtypes: absolute, users, angle

    Arugments:
        * x,y,z -- the coordinates of the view point.  Default
            setting is to use two angles (x, y) and a distance (Z), but
            other settings are available using viewtype attribute (see DISLIN
            documentation).  In default setting x represents rotation in the
            XY-plane, y represents rotation in the YZ-plane, and z
            represents distance of viewpoint from the axis system.
    """
    vdict = {'absolute':'ABS','user':'USER','angle':'ANGLE'}
    dislin.view3d(x,y,z,vdict[viewtype])



def setFocus3D(x,y,z, focustype='absolute'):
    """Sets position of the 3D focus.

    valid focustypes: absolute, user
    """
    fdict = {'absolute':'ABS','user':'USER'}
    dislin.vfoc3d(x,y,z,fdict[focustype])



def setCameraRotation3D(ang):
    """Defines camera rotation around viewing axis in degrees clockwise."""
    dislin.vup3d(ang)


def setViewAngle3D(ang):
    """Defines the field of view angle for 3D plots in degrees."""
    dislin.vang3d(ang)


def setBorder3D():
    """Enables a box to be drawn around 3D axis systems."""
    dislin.box3d()


def setGrid3D(nlinesx, nlinesy, planetype='all'):
    """Adds grids to 3D axes.

    Valid planetypes: all, back, bottom
        * all: grids in xy,xz,yz planes
        * back: grids in xz and yz planes
        * bottom: grid only in xy plane
    """
    pdict = {'all':'ALL','back':'BACK','bottom':'BOTTOM'}
    dislin.grid3d(nlinesx, nlinesy, pdict[planetype])



def setClipping3D(system='world', eyevalues=(1,-1)):
    """Defines whether clipping is done in world or eye coordinate system.

    Arguments:
        * system: 'world' or 'eye'
        * eyevalues: if system='eye', the distances in absolute coordinates
            to clip front and back planes.  Negative value means infinity.
    """
    cdict = {'world':'WORLD','eye':'EYE'}
    dislin.clip3d(cdict[system])
    if system == 'eye':
        dislin.vclp3d(eyevalues[0],eyevalues[1])


#------------------------------------------------------------------------------
# 3D Lines, Vectors, and Curves

def drawLine3D(x0,y0,z0,x1,y1,z1):
    """Draws 3D line in absolute 3D coordinates."""
    dislin.strt3d(x0,y0,z0)
    dislin.conn3d(x1,y1,z1)


def drawVector3D(x0,y0,z0,x1,y1,z1, vtype='normal'):
    """Draws 3D vector in absolute 3D coordinates.

    Arguments:

    * x0,y0,z0,x1,y1,z1 : the starting an ending 3D coordinates of the vector

    * vtype -- the type of head drawn on the vector

    """
    dislin.vectr3(x0,y0,z0,x1,y1,z1, vectordict[vtype])


def drawSphere3D(x0,y0,z0, radius, hres, vres):
    """Draws sphere in absolute 3D coordinates."""
    dislin.sphe3d(x0,y0,z0, radius, hres, vres)


def drawCurve3D(xlist, ylist, zlist):
    """Draws curve in user 3D coordinates."""
    dislin.curv3d(xlist,ylist,zlist,len(xlist))

#------------------------------------------------------------------------------
# Surface plot parameters

def setNoHiddenLines():
    """Causes hidden lines to be drawn for surface plots."""
    dislin.nohide()


def setProtectSurfaces():
    """Protects surface from being overwritten.

    Applies to routines derived from SURFUN, SURMAT and SURFCE
    """
    dislin.shlsur()


def setSurfaceVisibility(visible='both'):
    """Determines whether top, bottom, or both parts of surface are visible.

    Applies to routines derived from SURFUN, SURMAT and SURFCE
    """
    vdict = {'both':'BOTH','top':'TOP','bottom':'BOTTOM'}
    dislin.survis(vdict[visible])



def setSurfaceColors(topcolor=-1,bottomcolor=-1):
    """Determines color of top and bottom of surfaces. Color = index.

    Applies to routines derived from SURFUN, SURMAT and SURFCE
    """
    dislin.surclr(topcolor, bottomcolor)



def setSurfaceShadingMode(mode='flat'):
    """Determines the type of shading applied to colored surfaces.

    valid modes: flat, smooth
        * smooth: can only be applied to raster output formats (screens, TIFF, png, etc).
    """
    sdict = {'flat':'FLAT','smooth':'SMOOTH'}
    dislin.shdmod(sdict[mode], 'SURFACE')



def setSurfaceMeshing(state='off',shading=1):
    """Sets additional mesh/grid lines for surfaces."""
    sdict = {'off':'OFF','on':'ON'}
    val = sdict[state]
    if not shading:
        val = 'ONLY'
    dislin.surmsh(val)


def setSurfaceMeshColor(clr=-1):
    """Defines color(index) for surface mesh/grid lines."""
    dislin.mshclr(clr)


def setSurfaceColorScale(low,high):
    """Sets color scale for 3D surface plots."""
    dislin.zscale(low,high)

#------------------------------------------------------------------------------
# Surface Drawing functions

def generateFunctionMatrix(xlist, ylist, zlist, nx, ny,\
                           defaultz=0, gridx=2, gridy=2, gridweight=2.0):
    """For a set of x,y,z values generate a function matrix of dim(nx,ny).

    Used in conjunction with drawRegularSurface.
    See DISLIN documentation, GETMAT function for detailed discussion.
    """
    dislin.mdfmat(gridx, gridy, gridweight)
    # to be passed in
    zmat = [defaultz]*(nx*ny)
    dislin.getmat(xlist, ylist, zlist, len(xlist),
                  zmat, nx, ny, defaultz)
    return zmat



def drawFunctionSurface(function, xgrid, ygrid, xinterp, yinterp):
    """Draw 3D surface grid, calculated from a given function.

    Arguments:
        * function: a python function object representing the function to be
            visualized
        * xgridx, ygrid: the distance in user coordinates between grid lines
            plotted on the fsuface.
        * xinterp, yinterp: the number of points between grid lines used
            to interpolate the surface.  More points means longer plot times.
"""
    if withjython:
        f = function.func_name
        dislin.surfun(f, xinterp, xgrid, yinterp, ygrid)
        return None
    dislin.surfun(function, xinterp, xgrid, yinterp, ygrid)



def setSurfaceSize(xmin, xmax, ymin, ymax):
    """Alternate way to set surface size for drawRegularSurface function."""
    dislin.sursze(xmin, xmax, ymin, ymax)



def drawRegularSurface(matrix, nx, ny, xinterp, yinterp):
    """Draws 3D surface from a 2D array.  Assumes linear grid in X,Y-plane.

    Arguments:
        * nx, ny -- number of rows and columns of matrix
        * xdensity, ydensity : the number of points between grid lines used
            to interpolate the surface.  More points means longer plot times.
    """
    dislin.surmat(matrix, nx, ny, xinterp, yinterp)



def drawIrregularSurface(xlist, ylist, zmatrix):
    """Draws 3D surface from arbitrary x,y,z lists. z can be a matrix."""
    dislin.surfce(xlist, len(xlist), ylist, len(ylist), zmatrix)


def drawShadedSurface(xlist, ylist, zmatrix):
    """Draws a 3D surface with shaded colors."""
    dislin.surshd(xlist, len(xlist), ylist, len(ylist), zmatrix)


def drawParametricSurface(function, umin, umax, ustep, tmin, tmax, tstep):
    """Draws surface from parametric function: x=f(u,t), y=g(u,t), z=h(u,t)

    Function should take two values, and an axis indicator:
        e.g. def func(u,t,axis):
                 ....
    If axis = 1, should return x values, axis = 2 should return y values,
        axis = 3 should return z values
    """
    if withjython:
        f = function.func_name
        dislin.surfcp(f, umin, umax, ustep, tmin, tmax, tstep)
        return None
    dislin.surfcp(function, umin, umax, ustep, tmin, tmax, tstep)



def drawIsoSurface(xlist, ylist, zlist, values, levels):
    """Draws isosurfaces of form f(x,y,z)=constant."""
    dislin.suriso(xlist, len(xlist),
                  ylist, len(ylist),
                  zlist, len(zlist), values, levels)


#------------------------------------------------------------------------------
# Functions for drawing projections of 2D graphics in 3D space

def initializePlane3D(ll = (-1,-1,0), lr=(1,-1,0), ur=(1,1,0)):
    """Setup 2D Plane in 3D space.

    Arguments:
        * ll, lr, ur -- absolute 3D coordinates of lower-left, lower-right,
            and upper-right corners of plane (the default coordinates
            give a xy-plane, perpendicular to the z-axis at absolute
            z-coordinate zero)
    """
    x0,y0,z0 = ll
    x1,y1,z1 = lr
    x2,y2,z2 = ur
    dislin.grfini(x0,y0,z0,x1,y1,z1,x2,y2,z2)



def terminatePlane3D():
    """Terminate 2D Plane in 3D space."""
    dislin.grffin()


#------------------------------------------------------------------------------
# 3D color functions -- 2D plots, with third dimension represented by color

def colorAxis3D(xlow,xhigh,xfirst,xstep,ylow,yhigh,yfirst,ystep,\
            zlow,zhigh,zfirst,zstep):
    """Setup 3D-color axes."""
    dislin.graf3(xlow,xhigh,xfirst,xstep,ylow,yhigh,yfirst,ystep,\
                  zlow,zhigh,zfirst,zstep)


def setColorAxisLengths(x,y,z):
    """Sets lengths (in plot coordinates) of 3-D color axes."""
    dislin.ax3len(x,y,z)


def suppressColorBar():
    """Suppresses plotting of color bars for 3D-color plots."""
    dislin.nobar()


def setColorBarWidth(width):
    """Sets width (in plot coordinates) of colobar."""
    dislin.widbar(width)


def setColorBarPositionHoriz(pos):
    """Sets horizontal position of color bar relative to axis system.

    Default is 85 plot coordinates to the right of axis system.
    Positive values move bar further to right (away from axis system),
    while negative values move bar closer to axis system.
    """
    dislin.vkxbar(pos)



def setColorBarPositionVert(pos):
    dislin.vkybar(pos)


def setUserColorBar(low,high,start,step, length, plotx, ploty, axisname='', \
                    tickoption='clockwise', direction='horizontal',log=0):
    """Allows user to setup a customized color bar.  see ZAXIS in DISLIN docs.

    Arguments:
        * low,high -- upper and lower limits of color bar.
        * start, stop -- start/step values for labels
        * length -- length of the color bar in plot coordinates
        * plotx, ploty -- posiition, in plot coordinates, of lower left corner of bar.
        * axisname -- title displayed for the color bar
        * tickoption -- determines direction for plotting ticks/labels ('clockwise', 'ctrclockwise').
        * direction -- direction of the color bar ('vertical','horizontal')
        * log -- whether axis should be logarithmic
    """
    tdict = {'clockwise':0,'ctrclockwise':1}
    ddict = {'vertical':0,'horizontal':1}
    if log:
        dislin.zaxlg(low,high,start,step,axisname,tdict[tickoption],
                     ddict[direction],plotx,ploty)
    else:
        dislin.zaxis(low,high,start,step,axisname,tdict[tickoption],
                     ddict[direction],plotx,ploty)



def setColorBarRange(start=1,end=254):
    """Sets range of colors used in 3D-color plot color bar."""
    dislin.colran(start,end)


def setNoZeroColor():
    """Suppresses 3D-color plotting of points with zero value."""
    dislin.nobgd()


def drawColorCurve(xseq, yseq, zseq):
    """See DISLIN docs CURVE3."""
    dislin.curve3(xseq,yseq,zseq,len(xseq))


def drawColorRow(xseq, y, zseq):
    """See DISLIN docs CURVX3."""
    dislin.curvx3(xseq, y, zseq, len(xseq))


def drawColorColumn(x, yseq, zseq):
    """See DISLIN docs CURVY3."""
    dislin.curvy3(x, yseq, zseq, len(yseq))


def drawColorMatrix(matrix, nrows, ncols, xinterp=1, yinterp=1):
    """Draw matrix as 3D-color graph.

    Arguments:
        * matrix -- array or list containing values
        * nrows, ncols -- dimensions of matrix
        * xinterp, yinterp -- the number of interpolations between grid lines
    """
    dislin.crvmat(matrix, nrows, ncols, xinterp, yinterp)



def setColorResolution(width=1,height=1):
    """Defines the dimensions of rectangles used in color plots."""
    dislin.setres(width,height)


def setAutoColorResolution(nx, ny):
    """Given # of pts in x,y directions, automatically sets color-plot resolution."""
    dislin.autres(nx,ny)


#------------------------------------------------------------------------------
# 3D Lighting

def turnLightingSystemOn():
    """Enables lighting values for surface plots."""
    dislin.light('ON')


def turnLightingSystemOff():
    """Disables lighting values for surface plots."""
    dislin.light('OFF')


def turnLightOn(ID):
    """Turns individual light on (1...8)."""
    dislin.litmod(ID, 'ON')


def turnLightOff(ID):
    """Turns individual light off (1...8)."""
    dislin.litmod(ID, 'OFF')


def setLightPosition(lightID, x,y,z, coordmode='absolute'):
    """Sets position of individual light.

    Arguments:
        * lightID: ID of light (1..8).
        * x,y,z: 3D coordinates of light.
        * coordmode:
            - valid coordmodes: absolure, user, angle
    """
    vdict = {'absolute':'ABS','user':'USER','angle':'ANGLE'}
    dislin.litpos(lightID, x,y,z, vdict[coordmode])



def setLightingOptions(lightID, val, otype):
    """Sets various lighting options.

    Modifies the ambient, diffuse and specular intensities and the constant,
    linear and quadratic attentuation factors of light sources. See DISLIN
    manual (LITOPT).
    """
    odict = {'ambient':'AMBIENT','diffuse':'DIFFUSE','specular':'SPECULAR',
             'constant':'CONSTANT','linear':'LINEAR','quadratic':'QUADRATIC'}
    dislin.litopt(lightID, val, odict[otype])



def setMaterialParameters(val, ptype):
    """Defines material parameters for ambient, diffuse, and specular lighting.

    See DISLIN manual (MATOPT).
    """
    pdict = {'ambient':'AMBIENT','diffuse':'DIFFUSE','specular':'SPECULAR',
             'exponent':'EXPONENT'}
    dislin.matopt(val, pdict[ptype])



def calculateLighting(x,y,z, xnormal, ynormal, znormal):
    """Calculates colr values for given point and the point normal.

    See DISLIN manual (GETLIT)
    """
    dummy = 0
    clr = dislin.getlit(x,y,z,xn,yn,zn,dummy)



#------------------------------------------------------------------------------
# Contour related functions

def drawContourLine(xlist, ylist, level):
    """Draw contour line at given xlist, ylist coordiantes.  Label given by level."""
    dislin.concrv(xlist, ylist, len(xlist), level)


def drawContour(xlist, ylist, zmatrix, level):
    """Draws contour for f(x,y) where f(x,y) values are given by zmatrix."""
    dislin.contur(xlist, len(xlist), ylist, len(ylist), zmatrix, level)


def drawMatrixContour(zmatrix, nrows, ncols, level):
    """Draw contour over current x,y axis values.

    See CONMAT function in DISLIN manual for more info.
    """
    dislin.conmat(zmatrix, nrows, ncols, level)


def drawShadedContours(xlist, ylist, zmatrix, levels):
    """Draws contours with colors between contour lines."""
    dislin.conshd(xlist, len(xlist), ylist, len(ylist), zmatrix, levels, len(levels))


def setContourFilling(mode='cell'):
    """Sets the filling mode for shaded contours.

    valid modes: cell, polygon.
    """
    mdict = {'cell':'CELL','polygon':'POLY'}
    dislin.shdmod(mdict[mode],'CONTUR')



def getContourPoints(xlist, ylist, zmatrix, level, maxpts, maxcurves):
    """Generates contour points."""
    xpts, ypts = [],[]
    ptlist = []
    ncrvs = 0
    dislin.conpts(xlist, len(xlist), ylist, len(ylist), zmatrix, levels,
                xpts, ypts, maxpts, ptlist, maxcurves, ncrvs)


def setContourLabels(mode='none', ndigits=1):
    """Determines the manner in which contour labels are drawn.

    valid modes: none, float, string
        * none: no contour labels plotted.
        * float: level values used for labels
        * string: labels defined by setContourLabelString used.
    """
    odict = {'none':'NONE', 'float':'FLOAT', 'string':'CONLAB'}
    dislin.labdig(ndigits, 'CONTUR')
    dislin.labels(odict[mode], 'CONTUR')



def setContourLabelString(text=''):
    """Defines a character string used for contour labels."""
    dislin.conlab(text)


def setContourLabelDistance(distance=500):
    """Sets distance between contour labels in plot coordinates."""
    dislin.labdis(distance, 'CONTUR')


def setContourLabelColor(clrindex):
    """Sets color of contour labels."""
    dislin.labclr(clrindex, 'CONTUR')


def setContourMode(interval, quotient):
    """See CONMOD in DISLIN docs."""
    dislin.conmod(interval, quotient)


def setContourLabelGap(gap):
    """Defines distance between contour lines and labels."""
    dislin.congap(gap)


#----------------------------------------------------------------------------
# Plot interaction functions


def sendBuffer():
    """Send's the DISLIN buffer to the screen."""
    dislin.sendbf()

def getMouseClick(plotcoords = 0):
    """Returns the coordinates of a single mouse click.

    If plotcoords != 0, returns values in "plot coordinates", otherwise
    returns values in user coordinates.
    """
    coords = dislin.csrpt1()
    if plotcoords:
        return coords
    else:
        return dislin.xinvrs(coords[0]), dislin.yinvrs(coords[1])

def getMouseClicks(plotcoords = 0):
    """Returns the coordinates of multiple mouse clicks.  Finishes when mouse button 2 is pressed.

    If plotcoords != 0, return values in "plot coordinates", otherwise
    returns values in user coordinates
    """
    nmax = 1000
    xlist, ylist = [-92171]*nmax,[-92171]*nmax
    nclicks = dislin.csrpts(xlist, ylist, nmax)
    xlist, ylist = xlist[:nclicks], ylist[:nclicks]
    if plotcoords:
        return xlist, ylist
    else:
        x = [dislin.xinvrs(i) for i in xlist]
        y = [dislin.yinvrs(i) for i in ylist]
        return x,y



#------------------------------------------------------------------------------
# Dislin QuickPlots

def qsetup(format='screen', filename='dis.out'):
    """Performs useful setup calls for 'quick' functions."""
    setFormat(format)
    setFilename(filename)
    setScreenMode()
    initialize()


def qline(xlist,ylist):
    """Interface to dislin quick line plot function"""
    dislin.qplot(xlist,ylist,len(xlist))


def qscatter(xlist,ylist):
    """Interface to dislin quick scatter function."""
    dislin.qplsca(xlist,ylist,len(xlist))


def qbar(xlist):
    """interface to dislin quick bar plot function."""
    dislin.qplbar(xlist, len(xlist))


def qpie(xlist):
    """interface to dislin quick pie plot function."""
    dislin.qplpie(xlist, len(xlist))


def qcolor3D(zmatrix, nx, ny):
    """interface to dislin quick 3d color plot function."""
    dislin.qplclr(zmatrix, nx, ny)


def qsurface(zmatrix, nx, ny):
    """interface to dislin quick surface plot function."""
    dislin.qplsur(zmatrix, ny, ny)


def qcontour(zmatrix, nx, ny, nlevels):
    """interface to dislin quick contour plot function."""
    dislin.qplcon(zmatrix, ny, ny, nlevels)



#------------------------------------------------------------------------------
# DislinStack

class DislinVCR(UserList.UserList):
    """List-like object for creating plots from a sequence of dislin/pydislin functions.

    A plot is drawn by using the 'play' function to execute the specified
    function, argument pairs.

    Items should be added as tuples of form (func, (args)).  You can use
    all the standard list methods to append, insert, delete, etc.
    """
    def play(self):
        for func, arg in self.data:
            func(*arg)



#------------------------------------------------------------------------------
# Log info

##  $Log: pydislin.py,v $
##  Revision 1.6  2002/04/23 18:18:30  pmagwene
##  Added ability to write out PNG/PDF as string buffers
##
##  Revision 1.5  2002/04/23 14:52:52  pmagwene
##  more optioninfo
##
##  Revision 1.4  2002/04/17 21:49:58  pmagwene
##  added Reggie Dugard's mylab demo example
##  small fix to pydislin.py
##
##  Revision 1.3  2002/04/14 05:11:56  pmagwene
##  Modified the format of INFOMODULE to take both a module name and a class name
##
##  Revision 1.2  2002/04/12 01:10:49  pmagwene
##  Implemented "mylabels" options for Axis submitted by Reggie Duggard
##
##  Revision 1.1.1.1  2002/04/11 15:19:07  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.2  2002/03/09 01:12:37  pmagwene
##  removed disipyldoc
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.9  2001/03/27 03:00:01  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
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
##  Revision 1.5  2001/03/19 22:57:09  pmagwene
##  lots of doc strings added
##
##  Revision 1.4  2001/03/19 08:53:01  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.3  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##
##  Revision 1.2  2001/03/10 03:53:00  pmagwene
##  Renamed contourplots.py --> contours.py
##  Broke pxdislin.py --> pxdislin.py, pxdislin3D.py
##  Removed Options subclasses (everything is directly a named option)
##  Cleanup after previous changes
##  Added revision control keywords
##
