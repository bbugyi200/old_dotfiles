from disipyl import pydislin

class Canvas:
    format = pydislin.setFormat.__doc__ + """
    -------
    Default = 'screen'
    """



    filename = \
    """The default filename for the DISLIN plot.

    Valid: string
    -------
    Default = 'dis.out'
    """



    fileoverwrite = \
    """Specifies whether or not to overwrite the output file without prompting.

    Valid: 0 or 1
    -------
    Default = 0 (false)
    """



    errorfile = \
    """The file to which DISLIN will write any error output. 

    Valid: string representing filename
    -------
    Default = dis.err
    """



    errordevice = pydislin.setErrorDevice.__doc__ + """
    -------
    Default = screen
    """



    windowapp = pydislin.setWindowApp.__doc__ + """
    -------
    Default = 0 (false)
    """



    windowgeom = pydislin.setWindowGeometry.__doc__ + """
    -------
    Default = (None, None, None, None)
    """



    windowsize = pydislin.setWindowSize.__doc__ + """
    -------
    Default = (None, None)
    """



    windowkey = pydislin.setWindowKey.__doc__ + """
    -------
    Default = 'return'
    """



    windowstore = pydislin.setWindowStore.__doc__ + """
    -------
    Default = 1
    """



    windowID = pydislin.setWindowID.__doc__ + """
    -------
    Default = 0
    """


    windowtype = \
    """Specifies the way in which DISLIN draws to a given window ID (handle)

    Valid: 'none', 'window' ,'pixmap', 'widget'
    -------
    Default = 'none'
    """



    windowhold = pydislin.setWindowHold.__doc__ + """
    -------
    Default = 'hold'
    """



    colormode = pydislin.setColorMode.__doc__ + """
    -------
    Default = 'full'
    """


    colortable = \
    """  Specifies the color table to use for the DISLIN plot.

    You can specify any of the standard DISLIN tables:
    Valid: small, vga, grescale, rainbow, violet, reverse grey,
                      reverse rainbow, reverse violet

    or
    (redlist, greenlist, bluelist) 3 sequences if you want to specify your own color table
    -------
    Default = 'rainbow'
    """


    screenmode = pydislin.setScreenMode.__doc__ + """
    -------
    Default = 'normal'
    """


    suppressmessages =  \
    """Specifies whether error messages are suppressed or not.

    Valid: 0 (not suppressed) or 1 (suppressed)
    -------
    Default = 1 
    """


    #----------------------------------------------------------------------------
    # related to multicanvases

class Canvas2UP(Canvas):
    xoffset = \
    """Horizontal offset used in calculating the position of plots on
    a multicanvas (Canvas2UP, Canvas4UP)
    Valid: 0 < float < 1
    Default: 0.08
    """

    yoffset = \
    """vertical offset used in calculating the position of plots on
    a multicanvas (Canvas2UP, Canvas4UP)
    Valid: 0 < float < 1
    Default: 0.8
    """

    xfraction = \
    """The horiz. fraction of the half or quarter of a divided Canvas that
    that a plot will try to fill on a multicanvas object (Canvas2UP, Canvas4UP)
    Valid: 0 < float < 1
    Default: 0.80
    """


    yfraction = \
    """The vert. fraction of the half or quarter of a divided Canvas that
    that a plot will try to fill on a multicanvas object (Canvas2UP, Canvas4UP)
    Valid: 0 < float < 1
    Default: 0.80
    """  


    nvert = \
    """Number of plots to draw along the vertical axis of the canvas.
    Valid: int
    """

    nhoriz = \
    """Number of plots to draw along the horizontal axis of the canvas.
    Valid: int
    """     

class Canvas4UP(Canvas2UP):
    pass
