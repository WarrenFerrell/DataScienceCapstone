modelSearch <-  function(model, pGram, lock = 0) {
    #browser()
    pred <- vector()
    if( is.null(model) )
        return( pred )
    else if( length(pGram) == 0 ) {
        for( i in seq_along(model)) {
			term <- names(model)[[i]]
	
            if( term != "#" && "gfreq" %in% names(model[[i]]) ) {
                #pred[[term]] <- model[[i]]$gfreq ^ lock
                pred <- c(pred, term)
            }
        }
        return( pred )    # return all suggestions
    }
    else
        pterm <- pGram[[1]]

    pred <- c(pred, modelSearch( model[[pterm]], pGram[-1], lock + 1)) #recurse for detected terms
	if( lock )
		pred <- c(pred, modelSearch( model[["#"]], pGram[-1], lock))
	else
		pred <- c(pred, modelSearch( model, pGram[-1], 0 ))

    #browser()
    return( pred )#return( tapply(pred, names(pred), sum) ) #combine all results by term
}

nGramModel.predict <- function(model, pGram = list(""), k = Inf) {
    pGram <- strsplit(cleanLine(pGram), split = " ")[[1]]
    pGram <- pGram[pGram != ""]
    if( length(pGram) > (k - 1) )                             # Longest predictions
		pGram <- pGram[ (length(pGram)- k + 1):length(pGram)] # are with 12 grams
    #sort(modelSearch(model, pGram), decreasing = TRUE)[1:15]
    modelSearch(model, pGram)[1:15]
}
