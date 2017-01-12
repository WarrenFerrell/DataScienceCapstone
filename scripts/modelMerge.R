# ModelMerge
# merge gramModels
# parallel = FALSE or number of levels to parallelize
# parallel not enabled as not efficient when creating new process to handle (needs to use side effects)
# sideEffects = TRUE modifies the first environment listed (saving some memory)

library(pryr);

mergeModels <- function( ..., sideEffects = FALSE, removeOld = FALSE) {
    mergeRec <- function(A, B) { # add all of B's terms to A
        for( term in ls(B) ) {
            A[[term]] <- if(is.null(A[[term]])) {
                            B[[term]]
                        } else {
                            if( term == 'gfreq' ) {
                                A[[term]] + B[[term]]

                            } else {
                                mergeRec(A[[term]], B[[term]])
                            }
                        }
        }
        return(A)
    }

    args <- list(...)
    startMem <- mem_used()
    ret <- if(sideEffects) args[[1]] else nGramModel(maxMem = args[[1]]$maxMem, minFreq = args[[1]]$minFreq)

    for(i in seq(sideEffects + 1, length(args))) {
        ret$model <- mergeRec(ret$model,args[[i]]$model)
    }
    ret$size <- if(sideEffects) ret$size + mem_used() - startMem
                else mem_used() - startMem
    return(ret)
}