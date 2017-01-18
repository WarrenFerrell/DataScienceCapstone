library(microbenchmark); library(fastmatch); library(grr)

set.seed(1)
x <- round(runif(5e+4, 1, 2e4))



# tSeq <- function() { microbenchmark(
#     for(i in seq_along(x)) {x[i]},
#     for(i in seq(1,length(x))) {x[i]},
#     for(i in 1:length(x))  {x[i]}
#     )
# }
# enableJIT(0)
# print(tSeq())
# enableJIT(1)
# print(tSeq())
# enableJIT(2)
# print(tSeq())
# enableJIT(3)
# print(tSeq())  #fastest by 3x with 1:length(x) being best

# tSeq2 <- function() { microbenchmark(
#     for(i in seq(1,length(x)-1)) {x[i]},
#     for(i in seq.int(1,length(x)-1)) {x[i]},
#     for(i in 1:(length(x)-1))  {x[i]}
# )
# }
# enableJIT(3)
# print(tSeq2())   #seq(1,length(x)-1) is fastest

len <- length(x)
lenM1 <- length(x)
tSeq3 <- function() { microbenchmark(
    for(i in seq(1,length(x)-1)) {x[i]},
    for(i in seq.int(1,length(x)-1)) {x[i]},
    for(i in 1:(length(x)-1))  {x[i]},
    for(i in seq(1,len-1)) {x[i]},
    for(i in seq.int(1,len-1)) {x[i]},
    for(i in 1:(len-1))  {x[i]},
    for(i in seq_along(x[-1])) {x[i]},
    for(i in seq(1,lenM1)) {x[i]},
    for(i in seq.int(1,lenM1)) {x[i]},
    for(i in 1:lenM1) {x[i]}
)
}
enableJIT(3)
print(tSeq3())   #1:lenM1 is fastest in general
