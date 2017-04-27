# Various experiments for R's data.table package
#
# Date: January 25, 2017
#
# Author: Naumaan Nayyar
# Source: https://github.com/Rdatatable/data.table/wiki/Getting-started
# Source: https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Source: http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#passing-datatable-column-names-as-function-arguments
# REMEMBER: As long as j-expression returns a list, each element of the list will be converted to a column in the resulting data.table. This makes j quite powerful, as we will see shortly.


# clear workspace
rm(list = ls())

# load libraries
library(data.table)
library(digest)  # for hash functions
library(microbenchmark)  # for timing

# initialize different data.tables
iris.dt <- data.table(iris)
M = matrix(1, nrow=100000, ncol=100)
DT = as.data.table(M)
DT1 <- data.table(name = c("John","Smith","Jane","Ruby","Emerald","Jasmine","Tulip"),
                 class = c(1,2,3))
mytestdata <- data.table(name=c("tom","john","tom","john","jim","jim","jack"),
                         len=c(10,15,12,23,3,12,3),
                         group=c("a","b","a","a","a","b","b"))
mytestdata1 <- data.table(x = c(1,2,3,4,5,6,1,2,3,4,5,6),
                          y = c(2,3.8,6.2,8.1,10.3,11.7,2.9,6.3,8.7,12.6,15.1,17.9),
                          group = c(1,1,1,1,1,1,2,2,2,2,2,2))
mydupedata <- data.table(x1 = c("tom","john","tom","john","jim","jim","jack"),
                         x1 = c("tom","john","tom","john","jim","jim","jack"),  # same col names and values
                         x2 = c(10,15,12,23,3,12,3),
                         x2 = c(11,16,13,24,4,14,5),  # same col name, different values
                         group1 = c("a","b","a","a","a","b","b"),
                         group2 = c("a","b","a","a","a","b","b"),  # different col name, same values
                         val = c(12,44,23,54,34,32,56))
mymissingdata <- data.table(x1 = c("tom","john","tom","john","jim","jim","jack"),
                            x2 = c(NA,"john",NA,"john",NA,NA,"jack"),  # same col names and values
                            x3 = c(10,NA,NA,23,NA,12,NA),
                            y1 = c(NA,16,NA,24,4,NA,NA),  # same col name, different values
                            y2 = c(NA,NA,NA,NA,NA,NA,NA),
                            y3 = c("a",NA,"a","a",NA,"b","b"),  # different col name, same values
                            y4 = c(12,44,23,54,34,32,56))

# Basic
iris.dt[, -"Sepal.Length"]  # exclude a column
iris.dt[, !"Sepal.Length"]  # same
iris.dt[-2, ]  # exclude a row
c(as.matrix(iris.dt))  # convert to long vector col by col (or use c(t(iris.dt)) if row by row desired)

# Access values from a column - get is slower than [[]]
microbenchmark(iris.dt[[1]])  # same as unname(unlist(iris.dt[, 1]))
microbenchmark(iris.dt[["Sepal.Length"]])  # USE "colname"
microbenchmark(iris.dt[,Sepal.Length])  # return vector, DO NOT use "colname"
microbenchmark(iris.dt[,.(Sepal.Length)])  # return data.table, DO NOT use "colname"

microbenchmark(iris.dt[,get("Sepal.Length")])

# Set column by reference
system.time(for (i in 1:1000) set(DT,i,1L,i))
system.time(for (i in 1:1000) DT[i,V1:=i])

# extract row corresponding to maximum value of column
iris.dt[which.max(Sepal.Length),]  # returns row
iris.dt[, .I[which.max(Sepal.Length)]]  # returns row index

# sort rows by maximum value of numeric column
iris.dt[sort(Sepal.Length, index.return=TRUE)$ix,]

# sort rows by maximum value of categorical column
iris.dt[, .SD, keyby=Species]

# extract row corresponding to maximum value of column in each group
microbenchmark(iris.dt[iris.dt[, .I[which.max(Sepal.Length)], by=Species]$V1])
microbenchmark(iris.dt[, .SD[which.max(Sepal.Length)], by=Species])

# extract all rows that have the same value in a column as one of the rows
microbenchmark(DT1[class == class[name == "John"], name])
microbenchmark(DT1[.(class[name == "John"]), on="class"])
microbenchmark(DT1[class == DT1[name == "John", class], name])

# extract all row indices (stems from REMEMBER point)
iris.dt[, .I]  # in data.table (useful for calculations, esp. percentile type)
iris.dt[, seq_len(.N), by=Species]

# rank names based on mean()-like summary function values within each group and
# present output in sorted format
microbenchmark(mytestdata[, .SD[, .(mean(len), .N), by=name][order(V1)], by=group][, myrank:= 1:.N, by=group])
microbenchmark(mytestdata[, .SD[, .(mean(len), .N), by=name][order(V1), rank := 1:.N], by=group][order(rank), .SD, by=group])
microbenchmark(mytestdata[, .SD[, .(mean(len), .N), by=name][, myrank:= frank(V1)], by=group][order(myrank), .SD, by=group])
microbenchmark(mytestdata[, .SD[, .(mean(len), .N), by="name"][order(V1), myrank := 1:.N], by = "group"])  # does not re-order

# summarize a column's values by a grouping column
mytestdata1[, as.list(summary(y)), by = "group"]

# remove duplicate rows
microbenchmark(unique(mydupedata, by = c("x1", "group1"), fromLast = TRUE))  # keep last entry for dupes
microbenchmark(mydupedata[which(!duplicated(mydupedata, by = c("x1", "group1"), fromLast = TRUE))])
mydupedata.deduped.rows <- unique(mydupedata, by = "x2")  # based on first x2 column

# remove duplicate columns
mydupedata[, which(!duplicated(t(mydupedata))), with = FALSE]  # by value (so, second x1 and group2 removed)
dups <- duplicated(sapply(mydupedata, digest))  # computes hash function of column
mydupedata[, which(!dups), with = FALSE]  # by value (so, second x1 and group2 removed)
mydupedata[, unique(names(mydupedata)), with = FALSE]  # by column name (so, second x1 and second x2 removed)
mydupedata.deduped.columns <- copy(mydupedata)
mydupedata.deduped.columns[,
              which(duplicated(names(mydupedata.deduped.columns))) := NULL]  # by column name (so, second x1 and second x2 removed)

# find number of entries with NA in each column (fast to slow)
nz.c0 <- lapply(mymissingdata, function(x) sum(is.na(x)))  # faster than is.na(my...) as original is list
nz.c1 <- colSums(is.na(mymissingdata))
nz.c2 <- mymissingdata[, colSums(is.na(.SD))]
nz.c3 <- mymissingdata[, lapply(.SD, function(x) sum(is.na(x)))]
nz.c4 <- apply(is.na(mymissingdata), 2, sum)  # faster because input is matrix
nz.c5 <- apply(mymissingdata, 2, function(x) sum(is.na(x)))  # slower because conversion of list to matrix needed
nz.c6 <- summary(mymissingdata)

# find number of entries with NA in each row (fast to slow)
nz.r1 <- rowSums(is.na(mymissingdata))
nz.r2 <- apply(is.na(mymissingdata), 1, sum)
nz.r3 <- apply(mymissingdata, 1, function(x) sum(is.na(x)))
