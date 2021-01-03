# option documentation

#----------------------------------------------------------------------------
from disipyl import pydislin

#----------------------------------------------------------------------------

class Page:
    fillcolor = pydislin.setPageFill.__doc__ + """
    -------
    Default = None
    """


    border = \
    """Specifies whether a thin border will be drawn around the page.

    Valid: 0 (no border) or 1 (border)
    Default = 0
    """


    width = \
    """The page width in plot coordinates.

    Valid: int
    Default = 3000
    """


    height = \
    """The page height in plot coordinates.

    Valid: int
    Default = 2200
    """


    scale = pydislin.setScaling.__doc__ + """
    -------
    Default = 1
    """


    scalingmode = pydislin.setScalingMode.__doc__ + """
    -------
    Default = 'down'
    """


#---------------------------------------------------------------------------
# Title

class Title:
    color =  pydislin.setColor.__doc__ + """
    -------
    Default = 'fore'
    """

    height =  \
    """ Sets the height of title text.

    Valid: int
    -------
    Default = 48
    """

    justify =  \
    """Sets the justfication of title text.

    Valid: 'left', 'right', 'center'
    -------
    Default = 'center'
    """

    offset =  \
    """Sets the spacing of the title relative to the axis system.
    Negative values move the title closer to the axis system.  Positive
    values move the title further away from the axis system.

    Valid: int
    -------
    Default = 0
    """

    position =  \
    """Set the position of the title relative to the axis system.

    Valid: 'above', 'below'
    -------
    Default = 'above'
    """
