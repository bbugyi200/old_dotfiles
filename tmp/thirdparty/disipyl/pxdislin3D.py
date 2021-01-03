"""Base classes for 3D disipyl objects.

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
##  $Id: pxdislin3D.py,v 1.1.1.1 2002/04/11 15:19:02 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""

#------------------------------------------------------------------------------

import pydislin as pdis

import utilities
from utilities import allExist

import pxdislin
from pxdislin import DislinException, Object, DrawObject, Axis, AxisSystem2D, Curve, Canvas

#------------------------------------------------------------------------------


class AxisSystem3D(AxisSystem2D):
    """Three-dimensional axis system class.
    
    * If arguments xaxis, yaxis, zaxis are given they should be Axis objects.
    
    """
    def __init__(self, xaxis=None, yaxis=None, zaxis=None, **keywords):
        AxisSystem2D.__init__(self, xaxis, yaxis, **keywords)
        if zaxis:
            self.addZ(zaxis)
        else:
            self.addZ(Axis())        
            
    def addZ(self, zaxis):
        self.zaxis = zaxis
        if isinstance(self.zaxis, Axis):
            self.zaxis(whichaxis='Z')
        else:
            raise DislinException('Non-valid Axis specified')
        
    def set_defaults(self):
        AxisSystem2D.set_defaults(self)
        self.setoptions(
            autoz = None,
            border = 0,
            camerarotation = 0,
            focuspoint = (0.0, 0.0, 0.0),
            focustype = 'user',
            gridposition = 'all',
            lengths3D = (2.0, 2.0, 2.0),
            viewangle = 28,
            viewpoint = (None, None, None),
            viewtype = 'angle',
        )


    def configure(self):
        AxisSystem2D.configure(self)
        l = self.lengths3D
        pdis.setAxisLengths3D(l[0],l[1],l[2])
        v = self.viewpoint
        if allExist(*v):
            pdis.setView3D(v[0],v[1],v[2], self.viewtype)
        f = self.focuspoint
        pdis.setFocus3D(f[0],f[1],f[2], self.focustype)
        pdis.setCameraRotation3D(self.camerarotation)
        pdis.setViewAngle3D(self.viewangle)
        if self.centered:
            pdis.centerAxis()   

    def draw(self):            
        self.configure()
        try:
            if not self.xaxis or not isinstance(self.xaxis, Axis):
                raise DislinException('No x-axis specified or non-valid Axis object')
            if not self.yaxis or not isinstance(self.yaxis, Axis):
                raise DislinException('No y-axis specified or non-valid Axis object')
            if not self.zaxis or not isinstance(self.zaxis, Axis):
                raise DislinException('No z-axis specified or non-valid Axis object')
        except AttributeError:
            raise DislinException("Missing axis object for: %s" % str(self))           
        self.xaxis.draw()
        self.yaxis.draw()
        self.zaxis.draw()
        self.postaxis()
        self.displayit()
        self.postdisplay()
        
    def displayit(self):
        xaxis, yaxis, zaxis = self.xaxis, self.yaxis, self.zaxis
        xlow,xhigh,xfirst,xstep = xaxis.min, xaxis.max,xaxis.tickstart,xaxis.tickstep
        ylow,yhigh,yfirst,ystep = yaxis.min, yaxis.max,yaxis.tickstart,yaxis.tickstep 
        zlow,zhigh,zfirst,zstep = zaxis.min, zaxis.max,zaxis.tickstart,zaxis.tickstep 
        
        if self.autoscaling:
            if self.autox:
                pdis.autoAxisScaling(self.autox, 'X')
            if self.autoy:
                pdis.autoAxisScaling(self.autoy, 'Y')
            if self.autoz:
                pdis.autoAxisScaling(self.autoz, 'Z')
                
            dislin.graf3d(xlow,xhigh,xfirst,xstep,
                          ylow,yhigh,yfirst,ystep,
                          zlow,zhigh,zfirst,zstep)
            xlow, xhigh, xfirst, xstep = pdis.getAxisData('X')
            ylow, yhigh, yfirst, ystep = pdis.getAxisData('Y')
            zlow, zhigh, zfirst, zstep = pdis.getAxisData('Z')
            self.xaxis(min=xlow,max=xhigh,tickstart=xfirst,tickstep=xstep)
            self.yaxis(min=ylow,max=yhigh,tickstart=yfirst,tickstep=ystep)
            self.zaxis(min=zlow,max=zhigh,tickstart=zfirst,tickstep=zstep)
        else:    
            pdis.axis3D(xlow,xhigh,xfirst,xstep,
                        ylow,yhigh,yfirst,ystep,
                        zlow,zhigh,zfirst,zstep)
                        
    def limits(self, xmin, xmax, ymin, ymax, zmin, zmax):
        """A convenience function for quickly setting limits of x,y,z axes."""
        self.xaxis(min = xmin, max = xmax)
        self.yaxis(min = ymin, max = ymax)
        self.zaxis(min = zmin, max = zmax)
        
    def ticks(self, xstart, xstep, ystart, ystep, zstart, zstep):
        """A convenience function for quickly setting tickstart/step of x and y axes."""
        self.xaxis(tickstart = xstart, tickstep = xstep)
        self.yaxis(tickstart = ystart, tickstep = ystep)
        self.zaxis(tickstart = zstart, tickstep = zstep)         
                                
                        
    def postdisplay(self):
        if self.border:
            pdis.setBorder3D()
        if self.grids:
            pdis.setLineStyle(self.gridlinestyle)
            ng = self.ngridlines
            pdis.setGrid3D(ng[0],ng[1], self.gridposition)
            
#------------------------------------------------------------------------------
# 3D Curves and Symbols and Sphere

class Curve3D(Curve):
    """Curve drawn in three dimensions.
    
    * Interpolation is always linear for 3D curves
    """
    def __init__(self, xlist=[], ylist=[], zlist=[], **keywords):
        disfunc = pdis.drawCurve3D
        DrawObject.__init__(self,disfunc,**keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
        )
        
        
    def configure(self):
        Curve.configure(self)
        self.args = (self.xlist, self.ylist, self.zlist)
        

class Line3D(Curve3D):
    """Three dimensional line.
    
    """
    def __init__(self, startx, starty, startz, endx, endy, endz, **keywords):
        xl = [startx,endx]
        yl = [starty,endy]
        zl = [startz,endz]
        Curve3D.__init__(self, xl, yl, zl, **keywords)
                
    def set_defaults(self):
        Curve3D.set_defaults(self)
        self.setoptions( 
            symbols = 0,
        )
        


class Symbols3D(Curve3D):
    """Represents a set of symbols arranged in 3D space.
    
    Comments:
        * This is essentially a Curve3D drawn only by its points.
    """
    def set_defaults(self):
        Curve3D.set_defaults(self)
        self.__options__.extend(['size','shape'])
        self.size = 20
        self.shape = 'circle'
        self.symbols = -1   # This causes curve to only plot points
        
    def configure(self):
        self(symbolsize = self.size, symbolshape = self.shape)
        Curve3D.configure(self)



class Symbol3D(Symbols3D):
    """A single symbol drawn in 3D space."""
    def __init__(self, x, y, z, **keywords):
        Symbols3D.__init__(self, [x],[y],[z], **keywords)
        
 
 
class Sphere(DrawObject):
    """Three-dimensional sphere object."""
    def __init__(self, x, y, z, radius, **keywords):
        disfunc = pdis.drawSphere3D
        DrawObject.__init__(self, disfunc, **keywords)
        self.setdata( 
            x = x,
            y = y,
            z = z,
            radius = radius,
        )

    def set_defaults(self):
        DrawObject.set_defaults(self)
        self.setoptions( 
            color = 'fore',
            hres = 20,
            mesh = 1,
            meshcolor = 1,
            vres = 20,
        )


    def configure(self):
        if self.mesh:
            pdis.setSurfaceMeshing('on')
        pdis.setSurfaceMeshColor(self.meshcolor)
        pdis.setColor(self.color)        
        self.args = (self.x, self.y, self.z, self.radius, self.hres, self.vres)        
        
    def cleanup(self):
        pdis.setColor('fore')
        

#------------------------------------------------------------------------------
# SurfaceObjects

class SurfaceObject(DrawObject):
    """Base class for surface objects."""        

    def set_defaults(self): 
        DrawObject.set_defaults(self) 
        self.setoptions( 
            bottomcolor = -1,
            color = 'fore',
            colorscale = (None, None),
            hideparts = 1,
            mesh = 'on',
            meshcolor = -1,
            protected = 0,
            shadingmode = 'flat',
            shadingon = 1,
            topcolor = -1,
            visibility = 'both',
        )


    def configure(self):
        DrawObject.configure(self)
        if not self.hideparts:
            pdis.setNoHiddenLines()
        if self.protected:
            pdis.setProtectSurfaces()
        pdis.setSurfaceVisibility(self.visibility)
        pdis.setSurfaceColors(self.topcolor, self.bottomcolor)
        pdis.setSurfaceShadingMode(self.shadingmode)
        pdis.setSurfaceMeshing(self.mesh, self.shadingon)
        pdis.setSurfaceMeshColor(self.meshcolor)
        if allExist(*self.colorscale):
            pdis.setSurfaceColorScale(self.colorscale[0], self.colorscale[1])


class FunctionSurface(SurfaceObject):
    """Three dimensional surface representing z=f(x,y).
    
    Comments:
        * If you are going to try and pickle a plot which contains a FunctionSurface
            object, then be aware that the normal pickling rules apply with 
            reference to the function used.  Namely, to pickle/unpickle
            the function must be defined at the top level of a module 
            (by name reference, not storage of the implementation) 
            
    Arguments:
        * function -- a Python function of two arguments of the form z=f(x,y).
        
    """
    def __init__(self, function, xgrid, ygrid, xinterp, yinterp,  **keywords):
        disfunc = pdis.drawFunctionSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata(
            function = function,
            xinterp = xinterp,
            yinterp = yinterp,
            xgrid = xgrid,
            ygrid = ygrid,
        )
        
    def configure(self):
        SurfaceObject.configure(self)
        funcstr = str(self.function)
        self.args = (self.function, self.xgrid, self.ygrid,
                                    self.xinterp, self.yinterp)                                                            

                
class RegularSurface(SurfaceObject):
    """Object representing a surface drawn over a linear grid.
    
    Arguments:
      * matrix -- a Numeric array or a list with z-values
      * nx, ny -- the number of points on the grid in the x,y directions
      * xinterp, yinterp -- the number of points for interpolation between actual
        values
        
    By default the grid is draw at the scale of the current axis system.  
    This can be changed by setting the surfacelimits option where the
    arguments are (xmin, xmax, ymin, ymax).
    """
    def __init__(self, matrix, nx, ny,  **keywords):
        disfunc = pdis.drawRegularSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata(
            matrix = matrix,
            nx = nx,
            ny = ny,
        )
        
    def set_defaults(self):
        SurfaceObject.set_defaults(self)
        self.setoptions( 
            surfacelimits = (None,None,None,None),
            xinterp = 1,
            yinterp = 1,
        )


    def configure(self):
        SurfaceObject.configure(self)
        if allExist(*self.surfacelimits):
            pdis.setSurfaceSize(*self.surfacelimits)
        self.args = (self.matrix, self.nx, self.ny, self.xinterp, self.yinterp)
        

class RegularSurfaceXYZ(RegularSurface):
    def __init__(self, xlist, ylist, zlist, nx, ny,  **keywords):
        RegularSurface.__init__(self, None, nx, ny, **keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
        )
        
    def set_defaults(self):
        RegularSurface.set_defaults(self)
        self.setoptions(
            gridx = 2,
            gridy = 2,
            gridweight = 2.0,
            defaultz = 0,
        )


    def configure(self):
        m = pdis.generateFunctionMatrix(self.xlist, self.ylist,self.zlist,
                                        self.nx, self.ny, self.defaultz,
                                        self.gridx, self.gridy, self.gridweight)
        self.matrix = m
        RegularSurface.configure(self)                                        
             
    

class IrregularSurface(SurfaceObject):
    """Three-dimensional surface draw from irregularly spaced points."""
    def __init__(self, xlist, ylist, zmatrix,**keywords):
        disfunc = pdis.drawIrregularSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata(
            xlist = xlist,
            ylist = ylist,
            zmatrix = zmatrix,
        )
        
    def configure(self):
        SurfaceObject.configure(self)
        self.args = (self.xlist, self.ylist, self.zmatrix)      
        
                
class ShadedSurface(SurfaceObject):
    """Three-dimensional shaded (colored and lighted) surface object."""
    def __init__(self, xlist, ylist, zmatrix,**keywords):
        disfunc = pdis.drawShadedSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata( 
            xlist = xlist,
            ylist = ylist,
            zmatrix = zmatrix,
        )
        
    def configure(self):
        SurfaceObject.configure(self)
        self.args = (self.xlist, self.ylist, self.zmatrix)      
        

class ParametricSurface(SurfaceObject):
    """Three-dimensional representation of a parametric function of form f(u1,u2) = x,y,z
    
    Comments:
        * If you are going to try and pickle a plot which contains a ParametricSurface
            object, then be aware that the normal pickling rules apply with 
            reference to the function used.  Namely, to be able to pickle/unpickle
            the function must be defined at the top level of a module 
            (by name reference, not storage of the implementation) 
    """
    def __init__(self, function, urangestep, trangestep, **keywords):
        disfunc = pdis.drawParametricSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata(
            function = function,
            urange = urangestep,
            trange = trangestep,
        )
        

    def configure(self):
        SurfaceObject.configure(self)
        umin, umax, ustep = self.urange
        tmin, tmax, tstep = self.trange
        if None in self.urange or None in self.trange:
            raise DislinException("Ranges in ParametricSurface incorrectly set.")
        self.args = (self.function, umin, umax, ustep, tmin, tmax, tstep)        


class IsoSurface(SurfaceObject):
    """Three-dimensional isosurface. See DISLIN manual."""
    def __init__(self, xlist, ylist, zlist, values, levels, **keywords):
        disfunc = pdis.IsoSurface
        SurfaceObject.__init__(self,disfunc,**keywords)
        self.setdata( 
            xlist = xlist,
            ylist = ylist,
            zlist = zlist,
            values = values,
            levels = levels,
        )
        
    def configure(self):
        SurfaceObject.configure(self)
        self.args = (self.xlist, self.ylist, self.zlist, self.values, self.levels)     
    
    
#------------------------------------------------------------------------------
# Plane3D Object

class Plane3D(DrawObject, Canvas):
    """Class for drawing 2D plots as Planes in 3D.
    
    Inherits from Canvas so it can act as a canvas object to its plot.
    """
    def __init__(self, plot2d, **keywords):
        self.plot2d = plot2d
        disfunc = None
        DrawObject.__init__(self,disfunc,**keywords)
        
    def set_defaults(self): 
        DrawObject.set_defaults(self)
        self.setoptions( 
            color = 'fore',
            lowerleft = (-1, -1, 0),
            lowerright = (1, -1, 0),
            upperright = (1, 1, 0),
        )


    def draw(self):
        pdis.initializePlane3D(self.lowerleft, self.lowerright, self.upperright)
        self.plot2d.draw(self)
        pdis.terminatePlane3D()      

#------------------------------------------------------------------------------
# Lighting Objects
# Not working as of 09 March 2001

class LightingSystem(Object):
    """Container object for individual lights.  Can hold up to 8 lights."""
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)
        self.lights = [None,None,None,None,None,None,None,None]
        
    def addLight(self, light, ID):
        self.lights[ID-1] = light
               
    def set_defaults(self):
        Object.set_defaults(Object)
        self.setoptions(
            ison = 1,
        )
               
    def configure(self):
        if self.ison:
            pdis.turnLightingSystemOn()
        else:
            pdis.turnLightingSystemOff()
                    
    def draw(self):
        self.configure()
        for i in range(8):
            if self.lights[i] and self.lights[i].ison:
                pdis.turnLightOn(i+1)
            else:
                pdis.turnLightOff(i+1)
                
            
class Light(Object):
    """A light object for lighting 3D surfaces.  
    
    A light can not draw itself and must be assigned to a LightingSystem
    to be drawn.
    """
    def __init__(self, **keywords):
        Object.__init__(self, **keywords)

    def set_defaults(self):
        Object.set_defaults(Object)
        self.setoptions( 
            ID = None,
            coordinatesystem = 'user',
            ison = 1,
            lighttype = ('ambient', 0),
            material = ('ambient', 0.20),
            position = (None, None, None),
        )


    def configure(self):
        x,y,z = self.position
        if x:
            pdis.setLightPosition(self.ID, x,y,z, self.coordinatesystem)
        pdis.setLightingOptions(self.ID, self.lighttype[0], self.lighttype[1])
        pdis.setMaterialParameters(self.ID, self.material[0], self.material[1])
        
    def draw(self):
        msg = """Lights will not draw themselves.  Add lights to LightingSystem."""
        raise DislinException(msg)
        
    def toggle(self):
        if self.ison:
            self(ison = 0)
        else:
            self(ison = 1)
                    

#------------------------------------------------------------------------------
##  $Log: pxdislin3D.py,v $
##  Revision 1.1.1.1  2002/04/11 15:19:02  pmagwene
##  re-reimport into CVS ;)
##
##  Revision 1.4  2002/03/09 21:34:35  pmagwene
##  added quickplots
##
##  Revision 1.3  2002/03/09 01:12:37  pmagwene
##  removed disipyldoc
##
##  Revision 1.2  2002/01/29 02:51:32  pmagwene
##  changed to __options__ mechanism
##
##  Revision 1.1  2002/01/29 01:34:36  pmagwene
##  *** empty log message ***
##
##  Revision 1.1.1.1  2002/01/29 00:06:55  pmagwene
##  reimportation into jkim CVS
##
##  Revision 1.8  2001/04/24 13:53:36  pmagwene
##  added 3D Edges and GraphPlot
##
##  Revision 1.7  2001/04/05 14:44:20  pmagwene
##  Messed around with __setattr__ in base Object
##  Added miscplots.py - for useful plots that don't deserver to be
##   in plots.py (which should be rserved for the most basic plots)
##
##  Revision 1.6  2001/03/27 03:00:01  pmagwene
##  Modified versioning with revision tag.  Added a couple of new functions to AxisSystems (refactored this class as well)
##
##  Revision 1.5  2001/03/23 00:22:51  pmagwene
##  Some tweaknig of AxisSystem2D, Axis, and related functions to give better defaults.
##  Started mlabplot -- a set of functions with similar functionality to their counterparts in MATLAB.
##
##  Revision 1.4  2001/03/20 03:24:44  pmagwene
##  Added module for automatic documentation (disipyldoc.py)
##  Added doc strings to most objects.
##
##  Revision 1.3  2001/03/19 08:53:01  pmagwene
##  All basic objects wrapped.  Demos expanded.
##
##  Revision 1.2  2001/03/10 04:19:56  pmagwene
##  *** empty log message ***
##                    
