# Various experiments for R's data.table package
#
# Date: January 25, 2017
#
# Author: Naumaan Nayyar
# Source: https://github.com/Rdatatable/data.table/wiki/Getting-started
# Source: myd
# Source: https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Source: http://brooksandrew.github.io/simpleblog/articles/advanced-data-table/#passing-datatable-column-names-as-function-arguments
# REMEMBER: As long as j-expression returns a list, each element of the list will be converted to a column in the resulting data.table. This makes j quite powerful, as we will see shortly.


# clear workspace
rm(list = ls())

# load libraries
library(data.table)
library(microbenchmark)

# initialize different data.table
iris.dt <- data.table(iris)
M = matrix(1, nrow=100000, ncol=100)
DT = as.data.table(M)
DT1 <- data.table(name = c("John","Smith","Jane","Ruby","Emerald","Jasmine","Tulip"),
                 class = c(1,2,3))


# Access values from a column - get is slower than [[]]
microbenchmark(iris.dt[[1]])
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



