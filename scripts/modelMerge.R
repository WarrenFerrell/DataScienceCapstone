# ModelMerge
# merge gramModels
# parallel = FALSE or number of levels to parallelize
# parallel not enabled as not efficient when creating new process to handle (needs to use side effects)
# sideEffects = TRUE modifies the first environment listed (saving some memory)

library(pryr);

mergeModels <- function( ..., sideEffects = FALSE,
                         maxMem = 100) {

    mergeRec <- function(A, B) { # add all of B's terms to A w/o parallization

        for( term in ls(B) ) {
            A[[term]] <-  if(is.null(A[[term]])) {
                B[[term]] } else {
                    if( term != 'gfreq' ) {
                        mergeRec(A[[term]], B[[term]])
                    } else {
                        A[[term]] + B[[term]]
                    }
                }
        }
        return(A)
    }

    args <- list(...)
    startMem <- mem_used()
    ret <- if(sideEffects) args[[1]] else nGramModel(maxMem = args[[1]]$maxMem, minFreq = args[[1]]$minFreq)

    for(i in seq(sideEffects + 1, length(args))) {
        ret$model <- mergeRec(ret,args[[i]])
    }
    ret$size <- if(sideEffects) ret$size + mem_used() - startMem
                else mem_used() - startMem
    return(ret)
}



# formals(foreach)$.combine <- function(A,B) {
#
# }
# formals(foreach)$.packages <- c('tm','fastmatch')
# formals(foreach)$.inorder <- FALSE
# formals(foreach)$.verbose <- TRUE
# mergeRecPara <- function(A, B, parallel) {
#     A <- foreach( term = ls(B) ) %dopar% {
#         A[[term]] <-  if(is.null(A[[term]])) {
#             B[[term]] } else {
#                 if( term != 'gfreq' ) {
#                     mergeRec(A[[term]], B[[term]])
#                 } else {
#                     A[[term]] + B[[term]]
#                 }
#             }
#         }
#
#     return(A)
#     }
# }