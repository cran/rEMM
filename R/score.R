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


.simil_weight <- function(d, th) {
    w <- .5^(d/th - 1)
    w[d<=th] <-1
    w
}

## does newdata come from the EMM?
setMethod("score", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata, method=NULL, 
		match_cluster="nn", plus_one = FALSE, 
		initial_transition = FALSE) 
	score(x, as.matrix(rbind(newdata)), method, 
		match_cluster, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, method=NULL, 
		match_cluster="nn", plus_one = FALSE, 
		initial_transition = FALSE) 
	score(x, as.matrix(newdata), method, 
		match_cluster, plus_one, initial_transition)
)

setMethod("score", signature(x = "EMM", newdata = "matrix"),
	function(x, newdata, method = c(
			"product", 
			"log_sum", 
			"sum", 
			"weighted_product",
			"weighted_log_sum",
			"weighted_sum",
			"log_odds", 
			"supported_transitions",
			"supported_states",
			"weighted_supported_states",
			"sum_transitions"
			), 
		match_cluster = "nn", plus_one = FALSE, 
		initial_transition = FALSE) {

	method <- match.arg(method)

	    if(method == "product" 
		    || method == "log_sum" 
		    || method == "sum") {
		prob <- transition_table(x, newdata, method="prob", 
			match_cluster, plus_one, 
			initial_transition)[,3]
		
		if(method == "product")
		    return(prod(prob)^(1/length(prob)))
		if(method == "log_sum")
		    return(sum(log(prob))/length(prob))
		### must be sum
		return(sum(prob)/length(prob))
	    }

	    if(method == "log_odds") {
		log_odds <- transition_table(x, newdata, method="log_odds", 
			match_cluster, plus_one, 
			initial_transition)[,3]
		return(sum(log_odds))
	    }

	    if(method == "supported_transitions") {
		### We just ignore it
		###if(plus_one) warning("plus_one has no effect on supported transitions!")
		tTable <- transition_table(x, newdata, method="count",
			match_cluster, plus_one=FALSE, initial_transition)
		return((nrow(tTable)-sum(tTable[,3]==0))/nrow(tTable))
	    }
	    
	    if(method == "supported_states") {
		### We just ignore it
		###if(plus_one) warning("plus_one has no effect on supported transitions!")
		### match clusters should be "exact" for this to make sense
		if(!pmatch(match_cluster, "exact", nomatch=0)) warning("Using 'exact 'for 'match_cluster' for supported clusters!")
		states <- find_clusters(x, newdata, match_cluster="exact")
		return(sum(!is.na(states))/length(states))
	    }
	    
	    if(method == "sum_transitions") {
		cnt<-transition_table(x, newdata, method="counts")$counts
		return(sum(cnt)/sum(smc_countMatrix(x@tracds_d$mm))/length(cnt))
	    }

	    if(method == "weighted_product" 
		    || method == "weighted_log_sum"
		    || method == "weighted_sum"
		    || method == "weighted_supported_states") {
	   
		if(!pmatch(match_cluster, "nn", nomatch=0)) warning("Using 'nn 'for 'match_cluster' for weighted scores!")

		tTable <- transition_table(x, newdata, method="prob",
			match_cluster="nn", plus_one=plus_one,
			initial_transition)
	    
## Note: the last start is in the last row of tTable in column 2!
		n <- nrow(newdata)
		S <- numeric(n)
		for(i in 1:n) {
		    S[i] <- as.numeric(.simil_weight(
				    dist(
					    newdata[i, , drop=FALSE],
					    cluster_centers(x)[
					    tTable[min(i, n-1), 
					    if(i<n) 1 else 2], ,
					    drop=FALSE],
					    measure=x@measure),
				    #x@threshold
				    x@tnn_d$var_thresholds[
				    tTable[min(i, n-1),
				    if(i<n) 1 else 2]]
				    ))
		}


		if(method == "weighted_supported_states"){
		    return(sum(S)/length(S))
		}

		### product of source and target weight
		S <- S[-n] * S[-1]

		### probabilities
		P <- tTable[,3]

		if(method == "weighted_product") 
		    return(prod(S*P)^(1/nrow(tTable)))
		if(method == "weighted_log_sum") 
		    return(sum(log(S*P))/nrow(tTable))
		### must be weighted sum
		return(sum(S*P)/nrow(tTable))
	    }

	})


### score two models
setMethod("score", signature(x = "EMM", newdata = "EMM"),
	function(x, newdata, method=c("product", "log_sum", "sum",
			"supported_transitions"), 
		match_cluster="nn", plus_one = FALSE, 
		initial_transition = FALSE) {
	
	    method <- match.arg(method)


	    ### find transitions in newdata
	    trans <- transitions(newdata)	

	    ### match states in newdata to x
	    cl <- find_clusters(x,cluster_centers(newdata), 
		    match_cluster=match_cluster)
	    
	    ### translate to states in x
	    cl <- cbind(cl[as.integer(trans[,1])], cl[as.integer(trans[,2])])

	    ### FIXME: add weighted versions. What weights should we use?

	    if(method=="product") return(
		    prod(transition(x, cl, type="probability", 
				    plus_one=plus_one)^(1/nrow(cl))))
	    
	    if(method=="sum") return(
		    sum(transition(x, cl, type="probability", 
				    plus_one=plus_one)*(1/nrow(cl))))

	    if(method=="log_sum") return(
		    sum(log(transition(x, cl, type="probability", 
					    plus_one=plus_one))*(1/nrow(cl))))

	    if(method=="supported_transitions") return(
		    (nrow(cl)-sum(transition(x, cl, type="count", 
					    plus_one=FALSE)==0))/nrow(cl))

	}
)

