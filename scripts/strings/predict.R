treeSearch <-  function(tree, pGram, lock = 0) {
    #browser()
    pred <- vector()
    if( is.null(tree) )
        return( pred )
    else if( length(pGram) == 0 ) {
        for( i in seq_along(tree)) {
			term <- names(tree)[[i]]
	
            if( term != "#" && "#" %in% names(tree[[i]]) ) {
                #pred[[term]] <- tree[[i]]$# ^ lock
                pred <- c(pred, term)
            }
        }
        return( pred )    # return all suggestions
    }
    else
        pterm <- pGram[[1]]

    pred <- c(pred, treeSearch( tree[[pterm]], pGram[-1], lock + 1)) #recurse for detected terms
	if( lock )
		pred <- c(pred, treeSearch( tree[["#"]], pGram[-1], lock))
	else
		pred <- c(pred, treeSearch( tree, pGram[-1], 0 ))

    #browser()
    return( pred )#return( tapply(pred, names(pred), sum) ) #combine all results by term
}

nGramTree.predict <- function(tree, pGram = list(""), k = Inf) {
    pGram <- strsplit(cleanLine(pGram), split = " ")[[1]]
    pGram <- pGram[pGram != ""]
    if( length(pGram) > (k - 1) )                             # Longest predictions
		pGram <- pGram[ (length(pGram)- k + 1):length(pGram)] # are with 12 grams
    #sort(treeSearch(tree, pGram), decreasing = TRUE)[1:15]
    treeSearch(tree, pGram)[1:15]
}
