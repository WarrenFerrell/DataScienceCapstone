nGramModel.predict <- function(model, pGram = list("")) {
    pGram <- tolower(pGram)
    if( is.character(pGram) & length(pGram) == 1 ){
        pGram <- as.character(strsplit(pGram, split = " ")[[1]])
        if( length(pGram) > 1)
            pGram <- pGram[ (length(pGram)-1):length(pGram)] # this is a temporary cheap fix
    }
    if( length(pGram) == 0 ) {
        freqSort <- c(0)
        termSort <- c("")
        
        for( l in seq_along(model) ){
            freq <- 1
            term <- ""
            if( ! is.numeric(model[[l]]) ){
                term <- names(model)[[l]]
                if( is.null((freq <- model[[l]]$gfreq)))
                    freq <- 1
            } 
            for( t in seq_along(freqSort) ) {
                if( freq > freqSort[[t]] ) {
                    freqSort <- append(freqSort, freq, after = t - 1)
                    termSort <- append(termSort, term, after = t - 1)
                    break
                } 
            }
        }
        return( termSort[1:5] )
    } else if ( pGram[[1]] %in% names(model) ) {
        pterm <- pGram[[1]]
        
        return( nGramModel.predict(model[[pterm]], pGram[-1]) )
    } else {
        return( nGramModel.predict(model, pGram[-1]) ) 
    }
}

load(file = "twitterGramModel.RData")
s <- ""
prompt <- "Enter an string to predict the next character (q to exit): "
while( (s <- readline(prompt=prompt)) != "q" ) {
	print(suppressWarnings(nGramModel.predict(twitter, as.character(s))))
}

