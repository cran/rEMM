setMethod("state_counts", signature(x = "tNN"),
	function(x) x@counts)

setMethod("state_centers", signature(x = "tNN"),
	    function(x) x@centers)


