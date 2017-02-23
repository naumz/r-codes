# load libraries
library(bit64)
library(data.table)

m = matrix(sample(1:10, size=1000000, replace=TRUE), nrow=500, ncol=200)
DF = as.data.frame(m)
DT = as.data.table(m)

# single loops
print(system.time(apply(m,2,function(x) x-1)))
print(system.time(for (i in 1:ncol(m)) m[,i] - 1))
print(system.time(for (i in 1:ncol(m)) DF[,i] - 1))
print(system.time(for (i in 1:ncol(m)) DT[,i] - 1))

# double loops
print(system.time(apply(m,2,function(x) x-m)))
print(system.time(for (i in 1:ncol(m)) for (j in 1:ncol(m)) m[,i] - m[,j]))
print(system.time(for (i in 1:ncol(m)) for (j in 1:ncol(m)) DF[,i] - DF[,j]))
print(system.time(for (i in 1:ncol(m)) for (j in 1:ncol(m)) DT[,i] - DT[,j]))
