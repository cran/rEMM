
## reset the EMM for new sequence
## reset is also done by an observation of all NAs
setMethod("reset", signature(x = "EMMLayer"), function(x) { 
		x@current_state <- as.character(NA)
		x
	}
)
