library(fastmatch)

modelSearch <-  function(model, pGram, lock = 0) {
    #browser()
    pred <- vector()
    if( is.null(model) )
        return( pred )
    else if( length(pGram) == 0 ) {
        for( term in ls(model) ) {
            branch = model[[term]]
            if(  is.environment(branch) && fmatch("gfreq", ls(branch), nomatch = FALSE) ) {
                #pred[[term]] <- model[[i]]$gfreq ^ lock
                pred[[term]] <- model[[term]][['gfreq']]
            }
        }
        return( sort(pred, decreasing = TRUE) )    # return all suggestions
    } else
        pterm <- pGram[[1]]

    pred <- c(pred, modelSearch( model[[pterm]], pGram[-1], lock + 1)) #recurse for detected terms
	if( length(pred) < 3 )  {
        if( lock )
    		pred <- c(pred, modelSearch( model[['na']], pGram[-1], lock))
    	else
    		pred <- c(pred, modelSearch( model, pGram[-1], 0 ))
	}
    #browser()
    return( pred )#return( tapply(pred, names(pred), sum) ) #combine all results by term
}

nGramModel.predict <- function(gramModel, pGramRaw = list(''), k = Inf, commonTerms) {
    if(inherits(gramModel, 'nGramModel'))
       model <- gramModel$model
    pGram <- strsplit(cleanLine(pGramRaw), split = '\\s+')[[1]][-1]  %>%
		fmatch( commonTerms ) %>% as.character
    if( length(pGram) > (k - 1) )                             # Longest predictions
		pGram <- pGram[ (length(pGram)- k + 1):length(pGram)] # are with k grams

    pred <- modelSearch(model, pGram)
    #sort(modelSearch(model, pGram), decreasing = TRUE)[1:15]

    names(pred) <- commonTerms[ as.numeric(names(pred)) ]
    pred[1:15]
}
