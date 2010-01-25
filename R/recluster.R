
## fixme: find medoids for hclust

## medoid is defined as the object of a cluster, whose average 
## dissimilarity to all the objects in the cluster is minimal
## min_{m\inC}(1/n_C sum_{i\inC\m}(d(i,m))
.find_medoids <- function(d, k, cl) {
	dm <- as.matrix(d)
	sapply(1:k, FUN =function(i){
			take <- cl==i    
			names(which.min(colSums(dm[take,take, drop=FALSE])))
		})
}

## hierarchical clustering
setMethod("recluster_hclust", signature(x = "EMM"),
	function(x, k=NULL, h=NULL,  method="average", 
		prune=NULL) {

		if(!is.null(prune)) x <- prune(x, count_threshold = prune, 
			transitions = FALSE)

		d <- dist(state_centers(x), method = x@measure)
		hc <- hclust(d, method=method)
		cl <- cutree(hc , k=k, h=h)

		## if only h was given
		if(is.null(k)) k <- max(cl)

		if(is(cl, "matrix")) x <- lapply(1:ncol(cl), 
			FUN=function(i) 
			{
				if(!x@centroids) 
				new_center <- state_centers(x)[.find_medoids(d, k, cl[,i]),]
				## centroids are handled by merge_states!
				else new_center <- NULL
				merge_states(x, cl[,i], 
					clustering=TRUE, new_center = new_center)
			})
		else{ 
			if(!x@centroids) 
			new_center <- state_centers(x)[.find_medoids(d, k, cl),]
			else new_center <- NULL
			x <- merge_states(x, cl, 
				clustering=TRUE,  new_center = new_center)
		}

		attr(x, "cluster_info") <- list(clustering=cl, dendrogram=hc)
		x
	}
)

## k-means (euclidean)
setMethod("recluster_kmeans", signature(x = "EMM"),
	function(x, k, ..., prune=NULL) {

		if(!is.null(prune)) x <- prune(x, count_threshold = prune, 
			transitions = FALSE)

		if(!identical(tolower(x@measure), "euclidean")) warning(
			paste("Using k-means implies Euclidean distances but the EMM uses:", 
				x@measure))

		cl <- kmeans(state_centers(x), centers = k, ...)

		x <- merge_states(x, cl$cluster, clustering=TRUE, new_center=cl$centers)

		attr(x, "cluster_info") <- cl
		x
	}
)

## Partitioning around medoids (k-medians)
setMethod("recluster_pam", signature(x = "EMM"),
	function(x, k, ..., prune=NULL) {

		if(!is.null(prune)) x <- prune(x, count_threshold = prune, 
			transitions = FALSE)

		d <- dist(state_centers(x), method = x@measure)
		cl <- pam(d, k=k, ...)

		medoids <- state_centers(x)[cl$medoids,]
		x <- merge_states(x, cl$clustering, clustering=TRUE, new_center=medoids)

		attr(x, "cluster_info") <- cl
		x
	}
)

