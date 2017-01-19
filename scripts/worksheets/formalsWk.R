library(microbenchmark); library(foreach)


set.seed(1)
x1e2 <- round(runif(1e+2, 1, 2e4))
x1e3 <- round(runif(1e+3, 1, 2e4))
x1e2List <- list()
smlLst <- list()
for(i in 1:3){
    smlLst[[i]] <- round(runif(3, 1, 2e4))
}
for(i in 1:5){
    x1e2List[[i]] <- round(runif(1e+2, 1, 2e4))
}
x1e3List <- list()
for(i in seq_along(x1e4)){
    x1e3List[[i]] <- x1e4
}

srtRadClosure <- function(x) { sort.int(x, method = "radix") }

# srtRadFormals <- function(x) {
#     formals(sort.int)$method <- "radix"
#     sort.int(x)
# }

# srtRadPartial <- function(x) {
#     sortRadix <- pryr::partial(sort.int, method = "radix")
#     sortRadix(x)
# }
#
# tDefaults <- function(x) { microbenchmark(
#     srtRadFormals(x),
#     srtRadClosure(x),
#     srtRadPartial(x),
#     sort.int(x, method = "radix"),
#     times = 500
#     )
# }
#print( tDefaults(x1e2) ) # Closure and actual call fastest

# Unit: microseconds
# expr       min        lq        mean    median         uq        max neval cld
# srtRadFormals(x) 25206.134 28464.407 30576.79535 29618.383 31985.1985 136563.414   500   c
# srtRadClosure(x)    18.568    23.310    38.64208    26.471    43.6555   2253.435   500 a
# srtRadPartial(x)   694.125   822.717   951.97695   883.952   959.4090   4042.669   500  b
# sort.int(x, method = "radix")    17.778    21.334    31.14537    24.100    42.0750    107.457   500 a



# tTopCompare <- function(x) { microbenchmark(
#     srtRadClosure(x),
#     sort.int(x, method = "radix"),
#     times = 500
#     )
# }
# print( tTopCompare(x1e4) ) # extremely close with closure being just a slight bit slower

# Unit: milliseconds
# expr      min       lq     mean   median       uq      max neval cld
# srtRadClosure(x) 70.84447 79.44793 88.90124 82.26058 85.42878 213.0632   500   a
# sort.int(x, method = "radix") 71.20674 79.15677 87.42338 81.93919 84.81485 198.3720   500   a

#foreach



foreach. <- function(...) { foreach(..., .multicombine = FALSE, .inorder = TRUE, .verbose = FALSE) }

ex. <- expression(srtRadClosure(l))

forseq <- function(x) {
    #browser()
    ex. <- expression( srtRadClosure(l) )
    foreach(l = x) %do% eval(ex.)
}
forpar <- function(x) {
    srtRadClosure <- function(x) { sort.int(x, method = "radix") }
    ex. <- expression( srtRadClosure(l) )

    foreach(l = x) %dopar% srtRadClosure(l)
}

doParallel::registerDoParallel(cores= 4)

l1 <- list()
for(i in seq_along(smlLst)) {l1[[i]] <- srtRadClosure(x = as.numeric(smlLst[[i]]))}
l2 <- lapply(smlLst, srtRadClosure)
l3 <- foreach.(l = smlLst) %do% { srtRadClosure(l) }
l4 <- foreach.(l = smlLst) %do% eval(ex.)
l5 <- forseq(smlLst)
l6 <- foreach.(l = smlLst) %dopar% { srtRadClosure(l) }
l7 <- forpar(smlLst)

cat( identical(l1,l2), identical(l1,l3),
     identical(l1,l4), identical(l1,l5),
     identical(l1,l6), identical(l1,l7)  )

tForEach <- function(x) { microbenchmark(
    #for(i in seq_along(x)) {srtRadClosure(as.numeric(x[[i]])) },
    lapply(x, srtRadClosure),
    foreach(l = x) %do% { srtRadClosure(l) },
    forseq(x),
    forpar(x),
    times = 100
)
}
compiler::enableJIT(3)
#print( tForEach(x1e2List) )  #lapply is fastest, forar

# TRUE TRUE TRUE TRUE TRUE TRUEUnit: milliseconds
# expr       min        lq      mean    median        uq max neval  cld
# for (i in seq_along(x1e2List)) {srtRadClosure(as.numeric(x1e2List[[i]])) }  3.668151  4.051559  4.557423  4.284448  4.684250 9.369684   100  b
# lapply(x1e2List, srtRadClosure)  1.871410  2.061828  2.191559  2.142620  2.216101 3.900447   100 a
# forseq() 20.424305 22.031417 23.604731 23.140158 24.381838 40.035178   100   c
# forpar() 29.917642 31.104211 32.740901 32.303026 33.773842 39.436659   100    d

# expr      min        lq      mean    median       uq       max neval cld
# lapply(x, srtRadClosure)   97.976  121.8775  141.4846  133.9275  154.075   284.840   100 a
# foreach(l = x) %do% {     srtRadClosure(l) } 2543.804 2732.6440 2999.8086 2879.2120 3059.953  5568.793   100  b
# forseq(x) 2528.002 2740.3475 3042.5425 2854.1260 3090.570  5801.090   100  b
# forpar(x) 7596.646 8322.5720 8920.4708 8576.9925 9106.375 14533.932   100   c



#print( tForEach(x1e3List) )
compiler::enableJIT(0)
srtRadClosure <- compiler::cmpfun(srtRadClosure)
print( tForEach(x1e2List) )

# TRUE TRUE TRUE TRUE TRUE TRUEUnit: microseconds
# expr      min       lq      mean    median        uq      max neval cld
# lapply(x, srtRadClosure)  100.347  124.446  179.3354  135.3095  168.8905 2859.064   100 a
# foreach(l = x) %do% {     srtRadClosure(l) } 2216.693 2418.767 2727.6149 2562.5700 2719.2115 5666.769   100  b
# forseq(x) 2178.372 2401.186 2669.0945 2555.4590 2773.3355 5020.843   100  b
# forpar(x) 5230.620 5617.583 6168.3393 5834.4725 6455.3125 9144.498   100   c












