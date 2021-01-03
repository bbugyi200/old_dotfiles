# option documentation

#----------------------------------------------------------------------------
from disipyl import pydislin

#----------------------------------------------------------------------------

class DrawObject:
    color = pydislin.setColor.__doc__ + """
-------
Default = 'fore'
"""

    ucoords =  """
    Specifies whether user or plot coordinates are used.

    Valid: 0 (plot coordinates) or 1 (user coordinates)
    -------
    Default = 1
    """



class Text(DrawObject):
    TeX =  """
    Specifies whether TeX fonts should be used for the Text object.

    Valid: 0 or 1
    -------
    Default = 0
    """

    font =  pydislin.setFont.__doc__ + """

    see setFont in pydislin.py and the DISLIN documentation
    -------
    Default = 'default'
    """

    fonttype =  """
    see pydislin.setFont
    -------
    Default = 'hardware'
    """

    height =  pydislin.setTextHeight.__doc__ + """

    Valid: int
    -------
    Default = 36
    """

    justify =  """
    Sets the justfication of strings in the Text object.

    Valid: 'left', 'right', 'center'
    -------
    Default = 'left'
    """

    position =  """
    Specifies the coordinate position at which the Text object will be drawn.

    Valid: (int, int)
    -------
    Default = (0, 0)
    """

    rotation =  pydislin.setTextAngle.__doc__ + """

    Valid: int [0,360]
    -------
    Default = 0
    """



class Symbol(DrawObject):

    rotation = pydislin.setSymbolRotation.__doc__ + """
    -------
    Default = 0
    """

    shape =  """
    Specifies the shape of the symbol

    Valid: %s
    -------
    Default = 'circle'
    """ % str(pydislin.symboldict.keys())


    size =  pydislin.setSymbolSize.__doc__ + """
    -------
    Default = 20
    """

class LabeledSymbol(Symbol):

    labelheight =  """
    The size of the character used to label symobls in a LabeledSymbol.

    Valid: int
    -------
    Default = 25
    """

    labeltext  = """
    The string used to in the label of the LabeledSymbol.

    Valid: string
    -------
    Default = ''
    """

    xoffset =  """
    Specifies the horizontal offset of the label relative to the symbol.

    Valid: int
    -------
    Default = -40
    """

    yoffset =  """
    Specifies the vertical offset of the label relative to the symbol.

    -------
    Default = 40
    """


class Line(DrawObject):
    style =  pydislin.setLineStyle.__doc__ + """
    -------
    Default = 'solid'
    """

    width =  pydislin.setLineWidth.__doc__ + """
    -------
    Default = 1
    """


class Vector(Line):
    vectortype =  """
    Specifies the type of arrowhead drawn on the vector.

    Valid: %s
    -------
    Default = 'normal'
    """ % str(pydislin.symboldict.keys())


class VectorField(Vector):
    pass



class Curve(DrawObject):

    interpolation = pydislin.setCurveInterpolation.__doc__ + """
    -------
    Default = 'linear'
    """

    splinenumber = """
    Defines the number of points used for splined curves.

    Valid: int
    -------
    Default = 200
    """

    splineorder = """
    Defines the order of the polynomial used to draw splined curves.

    Valid: int
    -------
    Default = 3
    """

    style = pydislin.setLineStyle.__doc__ + """
    -------
    Default = 'solid'
    """

    symbols = """
    Sets the symbols used to mark points on curves.

    If symbols = 0, no symbols are drawn, only lines.
    If symbols < 0, every n-th symbol is drawn, w/out lines.
    If symbols > 0, every n-th symbols is drawn, with lines.
    -------
    Default = 0
    """

    symbolshape = """
    Specifies the shape of symbols used to mark the curve.

    Valid: %s
    -------
    Default = 'triangle'
    """ % str(pydislin.symboldict.keys())


    symbolsize = """
    Specifies the size of symbols used to mark the curve.

    Valid: int
    -------
    Default = 30
    """

    width = pydislin.setCurveWidth.__doc__ + """

    Valid: int
    -------
    Default = 1
    """


class AreaBetweenCurves(DrawObject):

    pattern = pydislin.setShadingPattern.__doc__ + """

    Valid: %s
    -------
    Default = 'diagonal1'
    """ % str(pydislin.shadingdict.keys())

    outline = """
    Specifies whether an outline will be drawn around the shaded area.

    Valid: 0 (no outline) or 1 (w/outline)
    -------
    Default = 1
    """


class ErrorBars(DrawObject):

    horizontalbars = """
    Specifies whether horizontal error bars should be used.

    Valid: 0 (vertical error bars) or 1 (horizontal error bars)
    -------
    Default = 0
    """

    marker = """
    Specifies the symbol used to mark the center of the error bars.

    Valid: %s
    -------
    Default = None
    """ % str(pydislin.symboldict.keys())

    markersize = """
    Defines the size of the symbol used to mark the center of the error bars.
    -------
    Default = 30
    """

class GeometricFigure(DrawObject):

    linestyle = pydislin.setLineStyle.__doc__ + """
    -------
    Default = 'solid'
    """

    position = """
    Specifies the coordinates at which the GeometricFigure will be drawn.

    Valid: (int,int) or (float,float)
    -------
    Default = (0, 0)
    """


    pattern = pydislin.setShadingPattern.__doc__ + """

    Valid: %s
    -------
    Default = 'diagonal1'
    """ % str(pydislin.shadingdict.keys())


    outline = """
    Specifies whether an outline will be drawn around the shaded area.

    Valid: 0 (no outline) or 1 (w/outline)
    -------
    Default = 1
    """


class Bars(DrawObject):
    bordercolor = pydislin.setBarBorderColor.__doc__ + """
    -------
    Default = -1
    """


    direction = pydislin.setBarType.__doc__ + """
    -------
    Default = 'vertical'
    """
