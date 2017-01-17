nGramTree.sort <- function(tree) {
    freq <- vector('numeric',length(tree))
    if( length(tree) == 1 )
        return( tree )
    for( i in seq_along(tree) ) {
        if( names(tree)[[i]] == 'gfreq' ) {
            freq[[i]] <- Inf
        } else {
            tree[[i]] <- nGramTree.sort(tree[[i]])
            freq[[i]] <- if(!is.null(tree[[i]]$gfreq)) tree[[i]]$gfreq else NA
        }

    }
    return( tree[ order(freq, decreasing = TRUE) ] )
}


env2list <- function(env, sorted = TRUE) {
    ret <- list()
	for( vName in ls(env) ) {
	    v <- env[[vName]]
		if( is.environment(v) ) {
			ret[[vName]] <- env2list(v)
		} else {
			ret[[vName]] <- v
		}
	}
    if( sorted )
        return( nGramTree.sort(ret) )
    else
        return( ret )
}

