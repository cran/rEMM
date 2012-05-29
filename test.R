
m <- matrix(1, ncol=10, nrow=20)

dyn.load("src/matrix.so")

.Call("clear_matrix", m, 1:2, c(1L,1L))
.Call("clear_matrix", m, 0L, 0L)
.Call("clear_matrix", m, 10L, 11L)
.Call("clear_matrix", m, 21L, 1L)



library("rEMM")

ds <- synthetic_stream(n_train=0, n_test=10000)

plot(ds$test[1:1000,], pch = ds$sequence_test, col ="gray")
text(ds$model$mu[,1], ds$model$mu[,2], 1:10)

d <- ds$test

emm <- EMM(thr=.01)
emm
build(emm, d, verb=TRUE)

print(object.size(emm), units="auto")
print(object.size(emm@tracds_d$mm), units="auto")

compact(emm)
print(object.size(emm), units="auto")
print(object.size(emm@tracds_d$mm), units="auto")


emm2 <- prune(emm, count=20, compact=TRUE)
emm2
print(object.size(emm2), units="auto")
print(object.size(emm2@tracds_d$mm), units="auto")










