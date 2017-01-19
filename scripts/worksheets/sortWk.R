library(microbenchmark); library(compiler); library(grr)
compiler::enableJIT(3)

set.seed(1)
x <- round(runif(1e+2, 1, 2e4))
# tSort <- function() { microbenchmark(
#     sort.int(x, method = "shell"),
#     sort.int(x, method = "quick"),
#     sort.int(x, method = "radix"),
#     grr::sort2(x))
# }
# enableJIT(0)
# print( tSort() )
# enableJIT(1)
#
# print( tSort() )
# enableJIT(3)
# print( tSort() )


#print( tSort() ) # radix sort with JIT 2 performs best

xList <- list()
for(c in x){
    xList[[as.character(c)]] <- c
}

# tOrder <- function() { microbenchmark(
#     xList[order(x, method = "radix")],
#     xList[grr::order2(x)]
#     )
# }
# print(tOrder())  # order with radix faster

xMatrix <- list()
for(c in x){
    xMatrix[[as.character(c)]] <- x
}

tMatrix <- function() { microbenchmark(
    for(v in xMatrix) {sort(as.numeric(v))},
    for(i in seq_along(xMatrix)) {sort(xMatrix[[i]])},
    for(S in names(xMatrix)) {sort(xMatrix[[S]])},
    times = 500
    )
}

# print(tMatrix()) # v in Matrix is fastest
# Unit: milliseconds
# expr      min       lq     mean   median       uq       max neval cld
# for (v in xMatrix) {     sort(as.numeric(v)) } 2.195360 2.361286 2.594911 2.472100 2.605434  7.939559   500  a
# for (i in seq_along(xMatrix)) {     sort(xMatrix[[i]]) } 2.443854 2.621039 3.109725 2.741534 2.931360 87.443394   500   b
# for (S in names(xMatrix)) {     sort(xMatrix[[S]]) } 2.464397 2.623212 2.925065 2.743113 2.944990 21.766726   500  ab



# tAccess <- function() { microbenchmark(
#     for(v in xMatrix) {v},
#     for(i in seq_along(xMatrix)) {xMatrix[[i]]},
#     for(S in names(xMatrix)) {xMatrix[[S]]}
# )
# }
#
# print(tAccess())