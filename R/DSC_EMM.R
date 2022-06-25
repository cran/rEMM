### stream interface

# library(rEMM)
# library(stream)
#
# data("EMMTraffic")
# EMMTraffic
#
# stream <- DSD_Memory(EMMTraffic)
# stream
#
# ## create EMM
# emm <- DSC_EMM(measure="eJaccard", threshold=0.2)
#
# ## build model using EMMTraffic data (note that the EMM object is
# ## changed without assignment!)
# update(emm, stream, n = 12)
# emm
#
# reset_stream(stream)
# plot(emm, stream, method = "pca")
# get_centers(emm)
#
# e <- get_EMM(emm)
# plot(e)
# states(e)
# transitions(e)


DSC_EMM <- function(formula = NULL, threshold = 0.2, measure = "euclidean", distFun = NULL,
  centroids = identical(tolower(measure), "euclidean"),
  lambda = 0) {
    structure(
      list(
        description = "Extensible Markov Model",
        formula = formula,
        RObj = new("EMM_R", threshold, measure, distFun,
          centroids, lambda)
      ),
      class = c("DSC_EMM","DSC_Micro", "DSC_R", "DSC")
    )
  }


get_EMM <- function(dsc) dsc$RObj$rEMM_Obj
set_EMM <- function(dsc, x) dsc$RObj$rEMM_Obj <- x

EMM_R <- setRefClass("EMM_R", fields = list(rEMM_Obj = "ANY",
  colnames = "ANY"))

EMM_R$methods(
  cache = function() {
    stop("SaveDSC not implemented for DSC_EMM!")
  }
)

EMM_R$methods(
  initialize = function(...) {
    rEMM_Obj <<- EMM(...)
    colnames <<- NULL

    .self
  }
)

EMM_R$methods(
  cluster = function(newdata, ...) {
    rEMM::build(rEMM_Obj, newdata)
  }
)

EMM_R$methods(
  get_microweights = function() {
    cluster_counts(rEMM_Obj)
  }
)

EMM_R$methods(
  get_microclusters = function() {
    as.data.frame(cluster_centers(rEMM_Obj))
  }
)

