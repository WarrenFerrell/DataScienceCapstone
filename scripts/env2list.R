nGramTree.sort <- function(tree) {
    len <- length(tree)
    if( len == 1 )
        return( tree )
    freq <- vector('integer', len)
    for( i in 1:len ) {
        if( is.numeric(tree[[i]]) ) {
            freq[[i]] <- Inf
        } else {
            tree[[i]] <- nGramTree.sort(tree[[i]])
            freq[[i]] <- if(!is.null(tree[[i]][['#']])) tree[[i]][['#']] else NA
        }
    }
    return( tree[ order(freq, decreasing = TRUE, method = "radix") ] )
}


env2list <- function(env, sorted = FALSE) {
    ret <- vector('list')
    for( vName in ls(env, sorted = FALSE) ) {
        v <- env[[vName]]
        if( is.numeric(v) )
            ret[[vName]] <- v
        else
            ret[[vName]] <- env2list(v)
    }
    if( sorted )
        return( nGramTree.sort(ret) )
    else
        return( ret )
}

