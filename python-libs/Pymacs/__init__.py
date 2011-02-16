#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright © 2002, 2003 Progiciels Bourbeau-Pinard inc.
# François Pinard <pinard@iro.umontreal.ca>, 2002.

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.  */

"""\
Interface between Emacs Lisp and Python - Module initialisation.

A few symbols are moved in here so they appear to be defined at this level.
"""




from pymacs import Let, lisp

# Identification of version.

__package__ = 'Pymacs'
__version__ = '0.24-beta2'
