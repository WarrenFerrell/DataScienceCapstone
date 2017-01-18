library(pryr)
#gList <- env2list(grm1mTree)
m1 <- new.env()
m1[['a']] <- new.env()
m1[['a']][['#']] <- 9
m1[['a']][['b']] <- new.env()
m1[['a']][['b']][['#']] <- 8

m2 <- new.env()
#m2 <- m1
m2[['a']] <- new.env()
m2[['a']][['#']] <- 1
m2[['a']][['b']] <- new.env()
m2[['a']][['b']][['#']] <- 2
m2[['c']] <- new.env()
m2[['c']][['#']] <- 10

# s1 <- new.env()
# print(object_size(s1))
# s1[['#']] <- 1
# print(object_size(s1))
# s1[['a']] <- new.env()
# print(object_size(s1))
# s1[['b']] <- new.env()
# print(object_size(s1))


# library(foreach)
# env2listTestforeach <- function(env) {
#     env2list <- function(env) {
#         ret <- vector('list')
#         for( vName in ls(env, sorted = FALSE) ) {
#             v <- env[[vName]]
#             if( is.numeric(v) )
#                 ret[[vName]] <- v
#             else
#                 ret[[vName]] <- env2list(v)
#         }
#         return( ret )
#     }
#     foreach(vName = ls(env, sorted = FALSE),
#             .combine = c, .inorder = FALSE, .multicombine = TRUE) %dopar% {
#         ret <- vector('list')
#         v <- env[[vName]]
#         if( is.numeric(v) )
#             ret[[vName]] <- v
#          else
#              ret[[vName]] <- env2list(v)
#         return( ret )
#     }
# }

env2list <- function(env) {
    ret <- vector('list')
    for( vName in ls(env, sorted = FALSE) ) {
        v <- env[[vName]]
        if( is.numeric(v) )
            ret[[vName]] <- v
        else
            ret[[vName]] <- env2list(v)
    }
    return( ret )
}

env2listTest <- function(env) {
    ret <- vector('list')
    for( vName in ls(env, sorted = FALSE) ) {
        v <- env[[vName]]
        if( is.numeric(v) )
            ret[[vName]] <- v
        else
            ret[[vName]] <- env2listTest(v)
        rm(v)
    }
    return( ret )
}



tEnv <- function() { microbenchmark(
    env2list(gramTree$tree),
    env2listTest(gramTree$tree), times = 10L
)
}
enableJIT(3)
print(tEnv()) #is.numeric is faster than is.environment, no faster to initialize list size first (because it takes so long to find the list length