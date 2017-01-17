library(fastmatch)

treeSearch <-  function(tree, pGram, lock = 0) {
    #browser()
    pred <- vector()
    if( is.null(tree) )
        return( pred )
    else if( length(pGram) == 0 ) {
        for( term in ls(tree) ) {
            branch = tree[[term]]
            if(  term != 'NA' && term != 'gfreq' && fmatch("gfreq", ls(branch), nomatch = FALSE) ) {
                #pred[[term]] <- tree[[i]]$gfreq ^ lock
                pred[[term]] <- tree[[term]][['gfreq']]
            }
        }
        return( sort(pred, decreasing = TRUE) )    # return all suggestions
    } else
        pterm <- pGram[[1]]

    pred <- c(pred, treeSearch( tree[[pterm]], pGram[-1], lock + 1)) #recurse for detected terms
	if( length(pred) < 5 )  {
        if( lock )
    		pred <- c(pred, treeSearch( tree[['NA']], pGram[-1], lock))
    	else
    		pred <- c(pred, treeSearch( tree, pGram[-1], 0 ))
	}
    #browser()
    return( pred )#return( tapply(pred, names(pred), sum) ) #combine all results by term
}

nGramTree.predict <- function(gramTree, pGramRaw = list(''), k = Inf, commonTerms) {
    if(inherits(gramTree, 'nGramTree'))
       tree <- gramTree$tree
    pGram <- strsplit(cleanLine(pGramRaw), split = '\\s+')[[1]][-1]  %>%
		fmatch( commonTerms ) %>% as.character
    if( length(pGram) > (k - 1) )                             # Longest predictions
		pGram <- pGram[ (length(pGram)- k + 1):length(pGram)] # are with k grams

    pred <- treeSearch(tree, pGram)
    #sort(treeSearch(tree, pGram), decreasing = TRUE)[1:15]

    names(pred) <- commonTerms[ as.numeric(names(pred)) ]
    pred[1:15]
}
