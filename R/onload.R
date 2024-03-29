#######################################################################
# rEMM - Extensible Markov Model (EMM) for Data Stream Clustering in R
# Copyright (C) 2011 Michael Hahsler
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.


## add Euclidean Squared Distance

.onLoad <- function(libname, pkgname) {
  if (!pr_DB$entry_exists("Euclidean2"))
    pr_DB$set_entry(
      names = c("Euclidean2"),
      FUN = function(x, y)
        dist(x, y) ^ 2,
      PREFUN = NA,
      POSTFUN = NA,
      convert = pr_dist2simil,
      type = "metric",
      loop = FALSE,
      C_FUN = FALSE,
      abcd = FALSE,
      description = "Euclidean Squared Distance."
    )
}
