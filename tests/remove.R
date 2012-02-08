library("rEMM")
data("16S")

data <- Mollicutes16S+1
test <- Mollicutes16S[2:10,]+1

## create two EMMs for different data
emm <- EMM("Kullback", threshold=0.1, data=data)
emm


## remove two states
nstates(emm)
nclusters(emm)
e <- remove_clusters(emm, c("3","4"))
nstates(e)
nclusters(e)


## prune w/ copy and w/o copy
e <- prune(emm, 5)
nstates(emm)
nstates(e)

prune(emm, 5, copy=FALSE)
nstates(emm)

## adding new states again
clusters(e)
build(e, data)
clusters(e)

nclusters(e)
nstates(e)

## merge clusters
nclusters(emm)
e <- merge_clusters(emm, c("2", "3", "4", "5"))
nclusters(e)
nstates(e)
clusters(e)
