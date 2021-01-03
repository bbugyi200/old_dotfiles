# option information for pxdisln.PlotObject

class PlotObject:
    TeXmode = \
    """Specifies whether the TeX mode should be used for text objects.
    Since this doesn't adversely affect anything else, it's probably
    simplest to leave this on.
    Valid: 0 (off) or 1 (on)
    -------
    Default: 1
    """


    defaultcanvastype = \
    """Specifies the default Canvas type (Canvas or its descendants)
    which is created when a plot finds itself without a canvas object
    to draw on.
    Valid: Any object derived from Canvas
    -------
    Default: Canvas
    """
