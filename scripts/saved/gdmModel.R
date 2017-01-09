modelConststr <- function(model, gram, freq) {
    term = gram[[1]]
    if( length(gram) == 1 ) {
        model[[term]] <- list("gfreq" = freq)
        return( model )
    } else {
        model[[term]] <- modelConststr(model[[term]], gram[-1], freq)
        return( model )
    }
} 

gramOrder <-  function(gramFreq) { # order by gram length then frequency
    gramFreq <- gramFreq[order(gramFreq, decreasing = TRUE)]
    gramLength <- list()
    for( i in seq_along(gramFreq) ) {
        gram <- eval( parse(text = names(gramFreq[i])) )
        gramLength <- list( gramLength, list(length(gram)) ) 
    }
    return( gramFreq[order(unlist(gramLength))] )
} 

nGramModel.create <- function(gramFreq, depth = 12) {
    model <- structure(list(), class = "nGramModel")
    gramFreq <- gramOrder(gramFreq)
    for( i in seq_along(gramFreq) ) {
        gram <- eval(parse(text = names(gramFreq[i])))
        freq <- gramFreq[[i]]
        model  <- modelConststr(model, gram, freq) 
    }
    return ( model )
} 
