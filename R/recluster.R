
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

		d <- dist(cluster_centers(x), method = x@measure)
		hc <- hclust(d, method=method)
		cl <- cutree(hc , k=k, h=h)

		## if only h was given
		if(is.null(k)) k <- max(cl)

		if(is(cl, "matrix")) x <- lapply(1:ncol(cl), 
			FUN=function(i) 
			{
				if(!x@centroids) 
				new_center <- cluster_centers(x)[.find_medoids(d, k, cl[,i]),]
				## centroids are handled by merge_clusters!
				else new_center <- NULL
				merge_clusters(x, cl[,i], 
					clustering=TRUE, new_center = new_center)
			})
		else{ 
			if(!x@centroids) 
			new_center <- cluster_centers(x)[.find_medoids(d, k, cl),]
			else new_center <- NULL
			x <- merge_clusters(x, cl, 
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

		cl <- kmeans(cluster_centers(x), centers = k, ...)

		x <- merge_clusters(x, cl$cluster, clustering=TRUE, new_center=cl$centers)

		attr(x, "cluster_info") <- cl
		x
	}
)

## Partitioning around medoids (k-medians)
setMethod("recluster_pam", signature(x = "EMM"),
	function(x, k, ..., prune=NULL) {

		if(!is.null(prune)) x <- prune(x, count_threshold = prune, 
			transitions = FALSE)

		d <- dist(cluster_centers(x), method = x@measure)
		cl <- pam(d, k=k, ...)

		medoids <- cluster_centers(x)[cl$medoids,]
		x <- merge_clusters(x, cl$clustering, clustering=TRUE, new_center=medoids)

		attr(x, "cluster_info") <- cl
		x
	}
)

## reachability
setMethod("recluster_reachability", signature(x = "EMM"),
	function(x, h, ..., prune=NULL) {

		if(!is.null(prune)) x <- prune(x, count_threshold = prune, 
			transitions = FALSE)

		
                d <- as.matrix(dist(cluster_centers(x), method = x@measure))

                # get adjecency matrix and find all paths 
                a_mat <- d < h
                r_mat <- a_mat
                for(i in 1:size(x)) {
                    r_mat <- r_mat%*%a_mat
                    storage.mode(r_mat) <- "logical"
                }

                to_merge <- unique(apply(r_mat, MARGIN=1, FUN = which))
                to_merge <- lapply(to_merge, as.character)

                x_merged <- x
                for(i in 1:length(to_merge)) {
                    m <- to_merge[[i]]
                    if(length(m)>1) {
                        x_merged <- merge_clusters(x_merged, to_merge = m)
                    }
                }

                x_merged
	}
)
