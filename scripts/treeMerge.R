# TreeMerge
# merge gramTrees
# parallel = FALSE or number of levels to parallelize
# parallel not enabled as not efficient when creating new process to handle (needs to use side effects)
# sideEffects = TRUE modifies the first environment listed (saving some memory)

library(pryr);

mergeTrees <- function(..., sideEffects = FALSE, removeOld = FALSE) {
    mergeRec <- function(A, B) { # add all of B's terms to A
        for( term in ls(B, sorted = FALSE) ) {
            #browser(expr = (term == '1'))
            A[[term]] <- if(is.null(A[[term]])) {
                            B[[term]]
                        } else {
                            if( term == '#' ) {
                                A[[term]] + B[[term]]
                            } else {
                                mergeRec(A[[term]], B[[term]])
                            }
                        }
        }
        return(A)
    }

    args <- list(...)
    if( inherits(args[[1]], 'nGramTree') ) {
        ret <- if(sideEffects) args[[1]] else args[[1]]$copy()
        for(i in seq(2, length(args))) {
            ret$timesCleaned <- ret$timesCleaned + args[[i]]$timesCleaned
            ret$termsKept <- ret$termsKept + args[[i]]$termsKept
            ret$tree <- mergeRec(ret$tree, args[[i]]$tree)
        }
    } else {
        ret <- if(sideEffects) args[[1]] else new.env()
        for(i in seq(sideEffects + 1, length(args))) {
            ret <- mergeRec(ret, args[[i]])

        }
    }
    return(ret)
}
