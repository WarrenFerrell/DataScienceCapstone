modelConststr <- function(model, gram, depth = 1, depth.Limit = 10) {
    if( (length(gram) == 1) || depth > depth.Limit )
		return( model )

	term = gram[[1]]
    if( term %in% names(model) )
		model[[term]]$gfreq <- model[[term]]$gfreq + 1
	 else
		model[[term]]$gfreq <- 1
	model[[term]] <- modelConststr(model[[term]], gram[-1], depth + 1, depth.Limit)
	return( model )
}

nGramModel.sort <- function(model) {
	freq <- vector()
	if( length(model) == 1 )
		return( model )
	if( "gfreq" %in% names(model) )
		freq[[1]] <- Inf
    for( i in seq.int( 2, to = length(model)) ) { #first element will always be gfreq
        model[[i]] <- nGramModel.sort(model[[i]])
        freq[[i]] <- model[[i]]$gfreq
    }
    return( model[ order(freq, decreasing = TRUE) ] )
}


nGramModel.create <- function(corpus, depth = 10) {
	model <- structure(list(), class = "nGramModel")
    for( i in seq_along(corpus) ) {
		x <- strsplit(as.String(corpus[[i]][[1]]), " ")[[1]]
		for( j in seq_along(x) ) {
			model  <- modelConststr(model, x[j:length(x)], depth.Limit = depth)
		}

    }
    return ( nGramModel.sort( model ) )
}
