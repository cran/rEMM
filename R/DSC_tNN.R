### stream interface

# library(rEMM)
# library(stream)
#
# cl <- DSC_tNN()
# cl
# stream <- DSD_Gaussians()
#
# update(cl, stream, 100)
# cl
# get_centers(cl)
#
# get_weights(cl)
#
# plot(cl, stream)


DSC_tNN <-
  function(formula = NULL,
    threshold = 0.2,
    measure = "euclidean",
    centroids = identical(tolower(measure),
      "euclidean"),
    lambda = 0) {
    structure(
      list(
        description = "Threshold Nearest Neighbor clustering",
        formula = formula,
        RObj = new("tNN_R", threshold, measure, centroids, lambda)
      ),
      class = c("DSC_tNN", "DSC_Micro", "DSC_R", "DSC")
    )
  }

tNN_R <- setRefClass("tNN_R", fields = list(rEMM_Obj = "ANY",
  colnames = "ANY"))

tNN_R$methods(
  cache = function() {
    stop("SaveDSC not implemented for DSC_tNN!")
  }
)

tNN_R$methods(
  initialize = function(threshold, measure, centroids, lambda) {
    rEMM_Obj <<- tNN(threshold, measure, centroids, lambda)
    colnames <<- NULL

    .self
  }
)

tNN_R$methods(
  cluster = function(newdata, ...) {
    rEMM::cluster(rEMM_Obj, newdata)
  }
)

tNN_R$methods(
  get_microweights = function() {
    cluster_counts(rEMM_Obj)
  }
)

tNN_R$methods(
  get_microclusters = function() {
    as.data.frame(cluster_centers(rEMM_Obj))
  }
)
