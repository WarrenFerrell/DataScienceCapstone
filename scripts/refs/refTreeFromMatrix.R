treeConststr <- function(tree, gram, depth = 1, depth.Limit = 10) {
    if( (length(gram) == 1) || depth > depth.Limit )
		return( tree )

	term = gram[[1]]
    if( term %in% names(tree) )
		tree[[term]]$gfreq <- tree[[term]]$gfreq + 1
	 else
		tree[[term]]$gfreq <- 1
	tree[[term]] <- treeConststr(tree[[term]], gram[-1], depth + 1, depth.Limit)
	return( tree )
}

nGramTree.sort <- function(tree) {
	freq <- vector()
	if( length(tree) == 1 )
		return( tree )
	if( "gfreq" %in% names(tree) )
		freq[[1]] <- Inf
    for( i in seq.int( 2, to = length(tree)) ) { #first element will always be gfreq
        tree[[i]] <- nGramTree.sort(tree[[i]])
        freq[[i]] <- tree[[i]]$gfreq
    }
    return( tree[ order(freq, decreasing = TRUE) ] )
}


nGramTree.create <- function(corpus, depth = 10) {
	tree <- structure(list(), class = "nGramTree")
    for( i in seq_along(corpus) ) {
		x <- strsplit(as.String(corpus[[i]][[1]]), " ")[[1]]
		for( j in seq_along(x) ) {
			tree  <- treeConststr(tree, x[j:length(x)], depth.Limit = depth)
		}

    }
    return ( nGramTree.sort( tree ) )
}
