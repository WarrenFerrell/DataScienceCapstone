# ModelMerge
# merge gramModels
# parallel = FALSE or number of levels to parallelize
# parallel not enabled as not efficient when creating new process to handle (needs to use side effects)
# sideEffects = TRUE modifies the first environment listed (saving some memory)

library(pryr);

mergeModels <- function( ..., sideEffects = FALSE, removeOld = FALSE) {
    mergeRec <- function(A, B, sizePtr) { # add all of B's terms to A

        for( term in ls(B) ) {
            #browser(expr = (term == '1'))
            A[[term]] <- if(is.null(A[[term]])) {
                            B[[term]]
                        } else {
                            if( term == 'gfreq' ) {
                                sizePtr[['*']] <- sizePtr[['*']] - 160
                                A[[term]] + B[[term]]

                            } else {
                                sizePtr[['*']] <- sizePtr[['*']] - (160 * 2.75)
                                mergeRec(A[[term]], B[[term]], sizePtr)
                            }
                        }
        }
        return(A)
    }

    args <- list(...)
    ret <- if(sideEffects) args[[1]] else args[[1]]$copy()
    sizePtr <- new.env()
    mergeRec <- compiler::cmpfun( mergeRec )

    for(i in seq(2, length(args))) {
        ret$size <- ret$size + args[[i]]$size
        sizePtr[['*']] <- ret$size
        ret$model <- mergeRec(ret$model, args[[i]]$model , sizePtr)
        ret$timesCleaned <- ret$timesCleaned + args[[i]]$timesCleaned
        ret$size <- sizePtr[['*']]
        if(removeOld)
            rm(args[[i]])
    }
    if( ret$size >  ret$maxSize ) {
        ret$size <- cleanModel(ret$model, minFreq) * 160
        ret$timesCleaned <- ret$timesCleaned + 1
    }

    return(ret)
}