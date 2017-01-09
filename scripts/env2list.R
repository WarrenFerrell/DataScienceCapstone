nGramModel.sort <- function(model) {
    freq <- vector('numeric',length(model))
    if( length(model) == 1 )
        return( model )
    for( i in seq_along(model) ) {
        if( names(model)[[i]] == 'gfreq' ) {
            freq[[i]] <- Inf
        } else {
            model[[i]] <- nGramModel.sort(model[[i]])
            freq[[i]] <- if(!is.null(model[[i]]$gfreq)) model[[i]]$gfreq else NA
        }

    }
    return( model[ order(freq, decreasing = TRUE) ] )
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
        return( nGramModel.sort(ret) )
    else
        return( ret )
}

