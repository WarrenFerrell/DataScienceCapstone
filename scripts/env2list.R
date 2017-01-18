nGramTree.sort <- function(tree) {
    freq <- vector('numeric',length(tree))
    if( length(tree) == 1 )
        return( tree )
    for( i in seq_along(tree) ) {
        if( names(tree)[[i]] == '#' ) {
            freq[[i]] <- Inf
        } else {
            tree[[i]] <- nGramTree.sort(tree[[i]])
            freq[[i]] <- if(!is.null(tree[[i]][['#']])) tree[[i]][['#']] else NA
        }

    }
    return( tree[ order(freq, decreasing = TRUE, method = "radix") ] )
}


env2list <- function(env, sorted = FALSE) {
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

