# Copyright (C) 2016 Antonin Houska
#
# This file is part of PGQA.
#
# PGQA is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.

# PGQA is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.

# You should have received a copy of the GNU General Public License qalong
# with PGQA. If not, see <http://www.gnu.org/licenses/>.

all : pgqa.pdf pgqa.html pgqa.info

pgqa.pdf : pgqa.texi
	texi2pdf pgqa.texi

pgqa.html : pgqa.texi
	texi2html pgqa.texi

pgqa.info : pgqa.texi
	makeinfo  pgqa.texi

clean :
	GLOBIGNORE=pgqa.texi && rm pgqa.*
