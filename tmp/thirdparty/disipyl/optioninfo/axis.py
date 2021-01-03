# option information for pxdislin.Axis 
from disipyl import pydislin

class Axis:
    axisscaling = \
    """
    Determines the type of axis scaling used in the DISLIN plot.

    Valid: linear, log
    -------
    Default = 'linear'
    """


    labeljust = pydislin.setLabelJustification.__doc__ + """
    ------- 
    Default =  'auto'
    """


    labelorient = pydislin.setLabelOrientation.__doc__ + """ 
    ------- 
    Default = 'horizontal'
    """


    labelposition = pydislin.setLabelPosition.__doc__ + """ 
    ------- 
    Default = 'ticks'
    """


    labelprecision = \
    """Specifies the precision used in the text for the axis labels.

    Valid = 'auto' or int
    ------- 
    Default = 'auto'
    """


    labeltype = pydislin.setLabelType.__doc__ + """ 
    ------- 
    Default = 'float'
    """


    linecolor = \
    """Sets the color used to draw Axis lines and ticks.

    Valid: -1 (default color), [0,255] DISLIN color value
    ------- 
    Default = -1
    """

    max = \
    """The upper limit of the given Axis.

    Valid: float or int
    ------- 
    Default = 1
    """

    min = \
    """The lower limit of the given Axis.

    Valid: float or int
    ------- 
    Default = -1
    """

    mylabel = \
    """For custom user defined axis labels (see option labeltype).

    Valid: seq of label strings
    -------
    Default = []
    """

    name = \
    """The text(name) drawn next to the given axis.

    Valid: string
    ------- 
    Default = ''
    """

    namedist = pydislin.setAxisNameDistance.__doc__ + """

    Valid: int
    ------- 
    Default = 30
    """

    nameheight =  pydislin.setAxisNameHeight.__doc__ + """

    Valid: int
    ------- 
    Default = 36
    """

    namejustify = pydislin.setAxisNameJustification.__doc__ + """

    Valid: left, right, center
    ------- 
    Default = 'center'
    """

    noline = pydislin.noAxisLines.__doc__ + """ 
    Valid: 0 (don't suppress) or 1 (suppress)
    ------- 
    Default = 0
    """

    textcolor = \
    """Sets the color used to draw the text on the given Axis.

    Valid: -1 (current color) [0,255] Disin color
    ------- 
    Default = -1
    """

    ticklength = pydislin.setTickLength.__doc__ + """

    Valid: int
    ------- 
    Default = (None, None)
    """

    ticknumber = pydislin.setTickNumber.__doc__ + """

    Valid: int
    ------- 
    Default = 2
    """

    tickposition = pydislin.setTickPosition.__doc__ + """

    ------- 
    Default = 'sameaslabels'
    """

    tickstart = \
    """Sets the lower limit for ticks on the given axis.

    Valid: int or float
    ------- 
    Default = -1
    """

    tickstep = \
    """Sets the step value for drawing ticks on the given axis.
    ------- 
    Default = 1
    """

    whichaxis = \
    """Specifies the current axis which the options correspond to.

    Valid: X, Y, or Z
    """

#---------------------------------------------------------------------------
# AxisSystem

class AxisSystem:
    centered = \
    """Specifies whether the axis system is centered on the page.

    Valid: 0 (not centered) or 1 (centered)
    ------- 
    Default = 0
    """

    clipped = pydislin.disableAxisClipping.__doc__ + """

    Valid: 0 (no clipping) or 1 (clipping)
    ------- 
    Default = 1
    """

    framethickness = pydislin.setFrameThickness.__doc__ + """ 

    Valid: int
    ------- 
    Default = 0
    """

    integerlabels = pydislin.setIntegerLabels.__doc__ + """

    Valid: 0 (float labels) or 1 (integer labels)
    ------- 
    Default = 0
    """

    pageorigin = pydislin.setAxisPageOrigin.__doc__ + """

    Valid: (int, int)
    ------- 
    Default = (None, None)
    """

    pageposition = pydislin.setAxisPagePosition.__doc__ + """

    Valid: (int, int)
    ------- 
    Default = (None, None)
    """

    pagesize = pydislin.setAxisPageSize.__doc__ + """

    Valid: (int, int)
    ------- 
    Default = (None, None)
    """

    suppress =  pydislin.noAxisSystem.__doc__ + """

    Valid: 0 (don't suppress) or 1 (suppress)
    ------- 
    Default = 0
    """

#---------------------------------------------------------------------------
# AxisSystem2D

class AxisSystem2D(AxisSystem):
    autoscaling = pydislin.autoAxisScaling.__doc__ + """

    Valid: 0 (no autoscaling) or 1 (autoscaling)
    ------- 
    Default = 0
    """

    autox = \
    """Specifies autoscaling for the X axis.  See 'autoscaling'

    Valid: 0 or 1
    ------- 
    Default = None
    """

    autoy = \
    """Specifies autoscaling for the Y axis.  See 'autoscaling'
    ------- 
    Default = None
    """

    axisparts = pydislin.setAxisParts.__doc__ + """

    Note: arguments are given in order: lower x-axis, left y-axis,
    upper x-axis, right y-axis.
    ------- 
    Default = ('all', 'all', 'ticks', 'ticks')
    """

    axistype = pydislin.setAxisType.__doc__ + """
    ------- 
    Default = 'rectangular'
    """

    gridlinestyle = 'dot',

    grids =  \
    """Specifies whether a grid will be superimposed over the axis system.

    Valid: 0 (no grid) or 1 (grid)
    ------- 
    Default = 0
    """

    gridtype =  pydislin.setGrid.__doc__ + """
    ------- 
    Default = 'square'
    """

    ngridlines = \
    """Specifies the number of grid lines to draw along the horizontal
    and vertical axes of the AxisSystem.

    Valid: (int, int)
    ------- 
    Default = (3, 3)
    """

    originlines = pydislin.setOriginLines.__doc__ + """ 
    ------- 
    Default = None
    """

    originlinestyle = pydislin.setLineStyle.__doc__ + """

    [specifically for origin lines on AxisSystem]
    ------- 
    Default = 'dash'
    """

    squared = \
    """Causes scaling along X and Y axes to be equal.

    Valid: 0 (no squaring) or 1 (squaring)
    ------- 
    Default = 0
    """
