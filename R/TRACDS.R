#######################################################################
# rEMM - Extensible Markov Model (EMM) for Data Stream Clustering in R
# Copyrigth (C) 2011 Michael Hahsler
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

## creator for TRACDS
TRACDS <- function(lambda=0) {
    new("TRACDS", lambda=lambda)
}

## show
setMethod("show", signature(object = "TRACDS"),
	function(object) {
	    cat("tNN with", nstates(object), "states.\n")
	    invisible(NULL)
	})

setMethod("copy", signature(x = "TRACDS"),
	function(x) {

	    r <- new("TRACDS") 
	    
	    ## copy environment
	    r@tracds_d <- as.environment(as.list(x@tracds_d))
	
	    r
	})


setMethod("nstates", signature(x = "TRACDS"),
	function(x) smc_size(x@tracds_d$mm))

setMethod("states", signature(x = "TRACDS"),
	function(x) smc_states(x@tracds_d$mm))

setMethod("current_state", signature(x = "TRACDS"),
	function(x) x@tracds_d$current_state)

setMethod("transitions", signature(x = "TRACDS"),
	function(x) {
	    m <- smc_countMatrix(x@tracds_d$mm)
	    edges <- apply(which(m>0, arr.ind=T), 
		    MARGIN=2, FUN=function(x) colnames(m)[x])
	    colnames(edges) <- c("from", "to")
	    edges
	}
)

setMethod("rare_transitions", signature(x = "TRACDS"),
	function(x, count_threshold)
	transitions(x)[transition(x, transitions(x),
		type="counts") < count_threshold,]
	)

