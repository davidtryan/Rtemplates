
# install.packages("pander")
library(pander)



irisSummary <- apply(iris[, 1:4], 2, function(x) tapply(x, iris$Species, summary))
irisSummary <- lapply(irisSummary, do.call, what = rbind)

panderOptions("table.split.table", Inf)  ## don't split tables
pander(irisSummary[1:2])

pander(lapply(iris, summary))
