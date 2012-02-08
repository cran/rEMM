library("rEMM")
data("16S")

data <- Mollicutes16S+1
test <- Mollicutes16S[2:10,]+1

## create two EMMs for different data
emm <- EMM("Kullback", threshold=0.1, data=data)
emm



## TRACDS
nstates(emm)
states(emm)
current_state(emm)
transitions(emm)
rare_transitions(emm, 2)

## TRACDS
cluster_counts(emm)
cluster_centers(emm)
nclusters(emm)
clusters(emm)
last_clustering(emm)
rare_clusters(emm,2)

find_clusters(emm, test)

## score, predict et al
transition_table(emm, test)
transition_table(emm, test, plus_one=TRUE)

score(emm, test)
score(emm, test, plus_one=TRUE)
score(emm, test, method="sum")
score(emm, test, method="sum", plus_one=TRUE)

predict(emm, "1")
p <- predict(emm, "1", probabilities=TRUE)
p[p>0]
p <- predict(emm, "1", probabilities=TRUE, plus_one=TRUE)
table(p)

## reset
current_state(emm)
reset(emm)
current_state(emm)

## copy: these need to be false!
identical(emm@tnn_d, copy(emm)@tnn_d)
identical(emm@tracds_d, copy(emm)@tracds_d)

