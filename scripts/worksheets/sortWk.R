library(microbenchmark); library(fastmatch); library(grr)

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

enableJIT(3)
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

# tMatrix <- function() { microbenchmark(
#     for(v in xMatrix) {sort(as.numeric(v))},
#     for(i in seq_along(xMatrix)) {sort(xMatrix[[i]])},
#     for(S in names(xMatrix)) {sort(xMatrix[[S]])}
#     )
# }
#
# print(tMatrix())

tAccess <- function() { microbenchmark(
    for(v in xMatrix) {v},
    for(i in seq_along(xMatrix)) {xMatrix[[i]]},
    for(S in names(xMatrix)) {xMatrix[[S]]}
)
}

print(tAccess())