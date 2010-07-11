###################################################
### chunk number 1: 
###################################################
options(width = 70, prompt="R> ", digits=4)
### for sampling
set.seed(1234)


###################################################
### chunk number 2: 
###################################################
library("rEMM")
data(EMMTraffic)
EMMTraffic


###################################################
### chunk number 3: 
###################################################
emm <- EMM(measure="eJaccard", threshold=0.2)
emm <- build(emm, EMMTraffic)
size(emm)


###################################################
### chunk number 4: 
###################################################
state_counts(emm)


###################################################
### chunk number 5: 
###################################################
state_centers(emm)


###################################################
### chunk number 6: Traffic_graph
###################################################
plot(emm, method="graph")


###################################################
### chunk number 7: 
###################################################
transition_matrix(emm)


###################################################
### chunk number 8: 
###################################################
transition_matrix(emm, type="counts")
#transition_matrix(emm, type="log_odds")


###################################################
### chunk number 9: 
###################################################
transition(emm, "1", "2", type="probability")


###################################################
### chunk number 10: 
###################################################
predict(emm, n=2, current="2")


###################################################
### chunk number 11: 
###################################################
predict(emm, n=2, current="2", probabilities=TRUE)


###################################################
### chunk number 12: Traffic_r3
###################################################
emm_3removed <- remove_states(emm, "3")
plot(emm_3removed, method = "graph")


###################################################
### chunk number 13: Traffic_rt52
###################################################
emm_52removed <- remove_transitions(emm, "5", "2")
plot(emm_52removed, method = "graph")


###################################################
### chunk number 14: Traffic_m25
###################################################
emm_25merged <- merge_states(emm, c("2","5"))
plot(emm_25merged, method = "graph")


###################################################
### chunk number 15: Traffic_l
###################################################
emm_fading <- EMM(measure="eJaccard", threshold=0.2, lambda = 1)
emm_fading <- build(emm_fading, EMMTraffic)
plot(emm_fading, method = "graph")


###################################################
### chunk number 16: Traffic_lp
###################################################
emm_pruned <- prune(emm_fading, count_threshold=0.1)
plot(emm_pruned, method = "graph")


###################################################
### chunk number 17: 
###################################################
data("EMMsim")


###################################################
### chunk number 18: sim_data
###################################################
plot(EMMsim_train, col="gray", pch=EMMsim_sequence_train)
lines(EMMsim_test, col ="gray")
points(EMMsim_test, col="red", pch=5)
text(EMMsim_test, labels=1:nrow(EMMsim_test), pos=3)


###################################################
### chunk number 19: sim_graph
###################################################
emm <- EMM(measure="euclidean", threshold=0.1)
emm <- build(emm, EMMsim_train)
plot(emm, method="graph")


###################################################
### chunk number 20: sim_MDS
###################################################
plot(emm, method="MDS")


###################################################
### chunk number 21: sim_MDS2
###################################################
plot(emm, method = "MDS", data=EMMsim_train)


###################################################
### chunk number 22: 
###################################################
score(emm, EMMsim_test, method="prod", match_state="exact", plus_one=FALSE,
initial_transition = FALSE)
score(emm, EMMsim_test, method="sum", match_state="exact", plus_one=FALSE,
initial_transition = FALSE)


###################################################
### chunk number 23: 
###################################################
transition_table(emm, EMMsim_test, match_state="exact", plus_one=FALSE)


###################################################
### chunk number 24: 
###################################################
score(emm, EMMsim_test, method="prod")
score(emm, EMMsim_test, method="sum")


###################################################
### chunk number 25: sim_hc
###################################################
## find best predicting model (clustering)
k <- 2:10
emmc <- recluster_hclust(emm, k=k, method ="average") 
plot(attr(emmc, "cluster_info")$dendrogram)


###################################################
### chunk number 26: 
###################################################
sc <- sapply(emmc, score, EMMsim_test)
names(sc) <- k
sc


###################################################
### chunk number 27: sim_optc_graph
###################################################
plot(emmc[[which.max(sc)]], method="graph")


###################################################
### chunk number 28: sim_optc_MDS
###################################################
plot(emmc[[which.max(sc)]], data=EMMsim_train)


###################################################
### chunk number 29: 
###################################################
data(Derwent)
summary(Derwent)


###################################################
### chunk number 30: Derwent1
###################################################
plot(Derwent[,1], type="l", ylab="Gauged flow", 
main=colnames(Derwent)[1])


###################################################
### chunk number 31: Derwent_state_counts
###################################################
Derwent_scaled <- scale(Derwent)
emm <- EMM(measure="euclidean", threshold=3)
emm <- build(emm, Derwent_scaled)
#state_counts(emm)
#state_centers(emm)
plot(emm, method = "state_counts", log="y")


###################################################
### chunk number 32: Derwent_EMM1
###################################################
plot(emm)


###################################################
### chunk number 33: Derwent_EMM2
###################################################
rare_threshold <- sum(state_counts(emm))*0.005
rare_threshold
plot(prune(emm, rare_threshold))


###################################################
### chunk number 34: Derwent2
###################################################
catchment <- 1 
plot(Derwent[,catchment], type="l", ylab="Gauged flows", 
main=colnames(Derwent)[catchment])
state_sequence <- find_states(emm, Derwent_scaled)

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
### chunk number 35: Derwent3
###################################################
catchment <- 6 
plot(Derwent[,catchment], type="l", ylab="Gauged flow", 
main=colnames(Derwent)[catchment])

mark_states("11", state_sequence, Derwent[,catchment], col="blue", label="11")
mark_states("12", state_sequence, Derwent[,catchment], col="red", label="12")
#mark_states("9", state_sequence, Derwent[,catchment], col="green", label="9")
#mark_states("3", state_sequence, Derwent[,catchment], col="blue")


###################################################
### chunk number 36: 
###################################################
data("16S")

emm <- EMM("Kullback", threshold=0.1)
emm <- build(emm, Mollicutes16S+1)


###################################################
### chunk number 37: Mollicutes_graph
###################################################
plot(emm, method = "graph")
## start state for sequences have an initial state probability >0
it <- initial_transition(emm)
it[it>0]


