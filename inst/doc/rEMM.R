### R code from vignette source 'rEMM.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: rEMM.Rnw:127-130
###################################################
options(width = 70, prompt="R> ", digits=4)
### for sampling
set.seed(1234)


###################################################
### code chunk number 2: rEMM.Rnw:848-851
###################################################
library("rEMM")
data(EMMTraffic)
EMMTraffic


###################################################
### code chunk number 3: rEMM.Rnw:863-866
###################################################
emm <- EMM(threshold=0.2, measure="eJaccard")
build(emm, EMMTraffic)
size(emm)


###################################################
### code chunk number 4: rEMM.Rnw:876-877
###################################################
cluster_counts(emm)


###################################################
### code chunk number 5: rEMM.Rnw:882-883
###################################################
cluster_centers(emm)


###################################################
### code chunk number 6: Traffic_graph
###################################################
plot(emm, method="graph")


###################################################
### code chunk number 7: rEMM.Rnw:914-915
###################################################
transition_matrix(emm)


###################################################
### code chunk number 8: rEMM.Rnw:919-921
###################################################
transition_matrix(emm, type="counts")
#transition_matrix(emm, type="log_odds")


###################################################
### code chunk number 9: rEMM.Rnw:933-934
###################################################
transition(emm, "2", "1", type="probability")


###################################################
### code chunk number 10: rEMM.Rnw:941-942
###################################################
predict(emm, n=2, current="2")


###################################################
### code chunk number 11: rEMM.Rnw:947-948
###################################################
predict(emm, n=2, current="2", probabilities=TRUE)


###################################################
### code chunk number 12: Traffic_r3
###################################################
emm_3removed <- remove_clusters(emm, "3")
plot(emm_3removed, method="graph")


###################################################
### code chunk number 13: Traffic_rt52
###################################################
emm_52removed <- remove_transitions(emm, "5", "2")
plot(emm_52removed, method="graph")


###################################################
### code chunk number 14: Traffic_m25
###################################################
emm_25merged <- merge_clusters(emm, c("2","5"))
plot(emm_25merged, method="graph")


###################################################
### code chunk number 15: Traffic_l
###################################################
emm_fading <- EMM(threshold=0.2, measure="eJaccard", lambda = 1)
build(emm_fading, EMMTraffic)
plot(emm_fading, method="graph")


###################################################
### code chunk number 16: Traffic_lp
###################################################
emm_fading_pruned <- prune(emm_fading, count_threshold=0.1,
    clusters=TRUE, transitions=TRUE)
plot(emm_fading_pruned, method="graph")


###################################################
### code chunk number 17: rEMM.Rnw:1105-1106
###################################################
data("EMMsim")


###################################################
### code chunk number 18: sim_data
###################################################
plot(EMMsim_train, col="gray", pch=EMMsim_sequence_train)
lines(EMMsim_test, col ="gray")
points(EMMsim_test, col="red", pch=5)
text(EMMsim_test, labels=1:nrow(EMMsim_test), pos=3)


###################################################
### code chunk number 19: sim_graph
###################################################
emm <- EMM(threshold=0.1, measure="euclidean")
build(emm, EMMsim_train)
plot(emm)


###################################################
### code chunk number 20: sim_graphviz
###################################################
plot(emm, method="graph")


###################################################
### code chunk number 21: sim_MDS
###################################################
plot(emm, method="MDS")


###################################################
### code chunk number 22: sim_MDS2
###################################################
plot(emm, method = "MDS", data=EMMsim_train)


###################################################
### code chunk number 23: rEMM.Rnw:1301-1303
###################################################
score(emm, EMMsim_test, method="product", match_cluster="exact")
score(emm, EMMsim_test, method="sum", match_cluster="exact")


###################################################
### code chunk number 24: rEMM.Rnw:1318-1319
###################################################
transition_table(emm, EMMsim_test, match_cluster="exact")


###################################################
### code chunk number 25: rEMM.Rnw:1336-1337
###################################################
score(emm, EMMsim_test, method="supported_transitions", match_cluster="exact")


###################################################
### code chunk number 26: rEMM.Rnw:1346-1347
###################################################
score(emm, EMMsim_test, method="supported_transitions", match_cluster="nn")


###################################################
### code chunk number 27: rEMM.Rnw:1365-1369
###################################################
methods <- c("product", "weighted_product", "log_sum", 
    "weighted_log_sum","sum", "weighted_sum")
sapply(methods, FUN = function(m) 
    score(emm, EMMsim_test, method=m, plus_one=TRUE))


###################################################
### code chunk number 28: sim_hc
###################################################
## find best predicting model (clustering)
k <- 2:10
emmc <- recluster_hclust(emm, k=k, method ="average") 
plot(attr(emmc, "cluster_info")$dendrogram)


###################################################
### code chunk number 29: rEMM.Rnw:1406-1409
###################################################
sc <- sapply(emmc, score, EMMsim_test, "product")
names(sc) <- k
sc


###################################################
### code chunk number 30: sim_optc_graph
###################################################
plot(emmc[[which.max(sc)]], method="MDS")


###################################################
### code chunk number 31: sim_optc_MDS
###################################################
plot(emmc[[which.max(sc)]], method="MDS", data=EMMsim_train)


###################################################
### code chunk number 32: rEMM.Rnw:1460-1462
###################################################
data(Derwent)
summary(Derwent)


###################################################
### code chunk number 33: Derwent1
###################################################
plot(Derwent[,1], type="l", ylab="Gauged flow", 
main=colnames(Derwent)[1])


###################################################
### code chunk number 34: Derwent_cluster_counts
###################################################
Derwent_scaled <- scale(Derwent)
emm <- EMM(measure="euclidean", threshold=3)
build(emm, Derwent_scaled)
#cluster_counts(emm)
#state_centers(emm)
plot(emm, method = "cluster_counts", log="y")


###################################################
### code chunk number 35: Derwent_EMM1
###################################################
plot(emm, method="MDS")


###################################################
### code chunk number 36: Derwent_EMM2
###################################################
rare_threshold <- sum(cluster_counts(emm))*0.005
rare_threshold
plot(prune(emm, rare_threshold), method="MDS")


###################################################
### code chunk number 37: Derwent2
###################################################
catchment <- 1 
plot(Derwent[,catchment], type="l", ylab="Gauged flows", 
main=colnames(Derwent)[catchment])
state_sequence <- find_clusters(emm, Derwent_scaled)

mark_states <- function(states, state_sequence, ys, col=0, label=NULL, ...) {
    x <- which(state_sequence %in% states)
    points(x, ys[x], col=col, ...)
    if(!is.null(label)) text(x, ys[x], label, pos=4, col=col)
}

mark_states("11", state_sequence, Derwent[,catchment], col="blue", label="11")
mark_states("12", state_sequence, Derwent[,catchment], col="red", label="12")
#mark_states("9", state_sequence, Derwent[,catchment], col="green", label="9")
#mark_states("3", state_sequence, Derwent[,catchment], col="blue")


###################################################
### code chunk number 38: Derwent3
###################################################
catchment <- 6 
plot(Derwent[,catchment], type="l", ylab="Gauged flow", 
main=colnames(Derwent)[catchment])

mark_states("11", state_sequence, Derwent[,catchment], col="blue", label="11")
mark_states("12", state_sequence, Derwent[,catchment], col="red", label="12")
#mark_states("9", state_sequence, Derwent[,catchment], col="green", label="9")
#mark_states("3", state_sequence, Derwent[,catchment], col="blue")


###################################################
### code chunk number 39: rEMM.Rnw:1702-1706
###################################################
data("16S")

emm <- EMM(threshold=0.1, "Kullback")
build(emm, Mollicutes16S+1)


###################################################
### code chunk number 40: Mollicutes_graph
###################################################
plot(emm, method = "graph")
## start state for sequences have an initial state probability >0
it <- initial_transition(emm)
it[it>0]


