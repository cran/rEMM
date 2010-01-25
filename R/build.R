
## make  newdata a matrix (with a single row)
setMethod("build", signature(x = "EMM", newdata = "numeric"),
	function(x, newdata) build(x, as.matrix(rbind(newdata)))
)

setMethod("build", signature(x = "EMM", newdata = "data.frame"),
	function(x, newdata, verbose = FALSE) build(x, as.matrix(newdata), 
                verbose)
)

setMethod("build", signature(x = "EMM", newdata = "matrix"),
	function(x, newdata, verbose = FALSE) {

		## low level graph manipulations w/o copying (work on x@mm)
		.addEdge <- function(from, to, w=1) {
			x@mm@edgeL[[from]]$edges <<- c(x@mm@edgeL[[from]]$edges, 
				which(x@mm@nodes==to))
			x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<- w
		}

		.incWeight <- function(from, to) {
			x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight <<-
			x@mm@edgeData@data[[paste(from,to,sep="|")]]$weight +1
		}

		.addNode <- function(node) {
			x@mm@nodes <<- c(x@mm@nodes, node)
			x@mm@edgeL[[node]]$edges <<- numeric(0)
		}

		## aging is also implemented in fade.R
		## fixme: we might want to reduce the cluster variability (sum_x2)
		## or the cluster threshold also

		.fade <- function() {
			x@counts <<- x@counts * x@lambda_factor
			x@initial_counts <<- x@initial_counts * x@lambda_factor
			x@mm@edgeData@data <<- lapply(x@mm@edgeData@data, FUN=function(z) {
					z$weight <- z$weight* x@lambda_factor
					z
				})
		}


		## this allows us to add more objects at once
		if(nrow(newdata)>1) {
			for(i in 1:nrow(newdata)) 
                        {
                            if(verbose && i%%50==0) cat("Added", i, "observations -",size(x), "states.\n")
                            x <- build(x, newdata[i,, drop=FALSE])
                        }
                        
                        if(verbose) cat ("Done -",size(x), "states.\n")
			return(x)
		}

		## reset on all NAs
		if(all(is.na(newdata))) return(reset(x))

		## fade cluster structure?
		#if(x@lambda>0) x <- fade(x)
		if(x@lambda>0) .fade()

		## first node?
		if(size(x)==0) {
			#x@mm <- addNode("1", x@mm)
			.addNode("1")
			x@current_state <- "1"
			x@initial_counts["1"] <- 1  

			rownames(newdata) <- "1"
			x@centers <- newdata
			x@counts["1"] <- 1 
			## initialize threshold
			x@var_thresholds["1"] <- x@threshold

		}else{

			## find a matching state
			sel <- find_states(x, newdata, match_state="exact")

			## NA means no match -> create a new node
			if(is.na(sel)) {
				## New node
				## get new node name (highest node number is last entry in count)
				sel <- as.character(as.integer(tail(names(x@counts),1)) + 1)

				#x@mm <- addNode(sel, x@mm)
				.addNode(sel)

				if(!is.na(x@current_state)) {
					#x@mm <- addEdge(x@current_state, sel, x@mm, 1)
					.addEdge(x@current_state, sel, 1)
					x@initial_counts[sel] <- 0  
				}else{
					x@initial_counts[sel] <- 1  
				}

				rownames(newdata) <- sel
				x@centers <- rbind(x@centers, newdata)
				#x@sum_x <- rbind(x@sum_x, newdata)
				#x@sum_x2 <- rbind(x@sum_x2, newdata^2)
				x@counts[sel] <- 1
				## initialize threshold
				x@var_thresholds[sel] <- x@threshold

				## update current_state
				x@current_state <- sel

			}else{ 
				## assign observation to existing node

				if(!is.na(x@current_state)) {
					## add edge or update weight
					if(isAdjacent(x@mm, x@current_state, sel)) {

						## this is slow and complains because the 
						## edge already exists
						#suppressWarnings(
							#x@mm <- addEdge(x@current_state, sel, x@mm,
								#as.numeric(edgeWeights(x@mm)[[x@current_state]][sel]) +1)
							#)
						.incWeight(x@current_state, sel)

					}else{
						## new edge
						#x@mm <- addEdge(x@current_state, sel, x@mm, 1)
						.addEdge(x@current_state, sel, 1)
					}
				}else{
					x@initial_counts[sel] <- x@initial_counts[sel]+1  
				}

				## update center (if we use centroids)
				if(x@centroids) {

					nnas <- !is.na(newdata)
					x@centers[sel,nnas] <- (x@centers[sel,nnas] * 
						x@counts[sel] + newdata[nnas])/(x@counts[sel]+1)
					nas <- is.na(x@centers[sel,])
					x@centers[sel,nas] <- newdata[nas]

					#nnas <- !is.na(newdata)
					## for sum_x and sum_x2 we have additivity
					#x@sum_x[sel,nnas] <- x@sum_x[sel,nnas] + newdata[nnas]
					#x@sum_x2[sel,nnas] <- x@sum_x2[sel,nnas] + newdata[nnas]^2
					#nas <- is.na(x@sum_x[sel,])
					#if(any(nas)) {
						#    x@sum_x[sel,nas] <- newdata[nas]
						#x@sum_x2[sel,nas] <- newdata[nas]^2
						#}
				}

				## update counts and current_state state
				x@current_state <- sel
				x@counts[sel] <- x@counts[sel] + 1
			}
		}

		x

	}
)
