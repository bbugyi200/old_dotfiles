#!/usr/bin/python
"""A module for generating disipyl documentation.

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

##  $Id: disipyldoc.py,v 1.1.1.1 2002/04/11 15:18:54 pmagwene Exp $

__author__ = "Paul M. Magwene <paul.magwene@yale.edu>"
__version__ = "$Revision: 1.1.1.1 $"
__credits__ = ""


#------------------------------------------------------------------------------

import pydoc

#------------------------------------------------------------------------------
import disipyl
import utilities

#----------------------------------------------------------------------------

from disipyl import pydislin, pxdislin, pxdislin3D, plots, complexplots, contours
from disipyl import utilities, demos, tkdisipyl, mlabplots, quickplots
import disipyldoc

modlist = [pydislin, pxdislin, pxdislin3D, plots, complexplots, contours,
           demos, tkdisipyl, mlabplots, quickplots, utilities]


def generateDisipylDocs():
    """Generate HTML documentation for all modules in global variable 'modlist'"""
    pydoc.writedoc('disipyl')
    for mod in modlist:
        pydoc.writedoc(mod.__name__)


def generateDislinDocs():
    pydoc.writedoc('dislin')




#------------------------------------------------------------------------------

if __name__ == '__main__':
    generateDisipylDocs()
    generateDislinDocs()
