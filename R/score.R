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


## does newdata come from the EMM?

setMethod("score", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_cluster="nn", plus_one = FALSE, 
		initial_transition = FALSE) 
	score(x, as.matrix(rbind(newdata)), method, 
		match_cluster, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, method = c("prod", "sum", "log_odds"), 
		match_cluster="nn", plus_one = FALSE, 
		initial_transition = FALSE) 
	score(x, as.matrix(newdata), method, 
		match_cluster, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "matrix"),
        function(x, newdata, method = c("prod", "sum", "log_odds"), 
                match_cluster="nn", plus_one = FALSE, 
                initial_transition = FALSE) {

            method <- match.arg(method)

            if(method == "prod") {
                prob <- transition_table(x, newdata, method="prob", 
                        match_cluster, plus_one, 
                        initial_transition)[,3]
                return(prod(prob)^(1/length(prob)))
            }

            if(method == "sum") {
                prob <- transition_table(x, newdata, method="prob", 
                        match_cluster, plus_one, 
                        initial_transition)[,3]
                return(sum(prob)/length(prob))

            }

            if(method == "log_odds") {
                log_odds <- transition_table(x, newdata, method="log_odds", 
                        match_cluster, plus_one, 
                        initial_transition)[,3]
                return(sum(log_odds))
            }
        })
