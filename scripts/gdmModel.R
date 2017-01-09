

gramOrder <-  function(gramFreq) { # order by gram length then frequency
    gramFreq <- gramFreq[order(gramFreq, decreasing = TRUE)]
    gramLength <- list()
    for( i in seq_along(gramFreq) ) {
        gram <- eval( parse(text = names(gramFreq[i])) )
        gramLength <- list( gramLength, list(length(gram)) )
    }
    return( gramFreq[order(unlist(gramLength))] )
}

modelConststr <- function(model, gram, freq) {
    #browser()
    term = gram[[1]]
    if( !exists(term, where = model) )
        model[[term]] <- new.env()

    if( length(gram) == 1 ) {
        model[[term]][['gfreq']] <- freq

        return( model )
    } else {
        model[[term]] <- modelConststr(model[[term]], gram[-1], freq)
        return( model )
    }
}

nGramModel.create <- function(gramFreq, depth = 12) {
    #browser()
    model <- structure(new.env(), class = 'nGramModel')
    gramFreq <- gramOrder(gramFreq)
    for( i in seq_along(gramFreq) ) {
        gram <- eval(parse(text = names(gramFreq[i]))) %>% as.character
        freq <- gramFreq[[i]]
        model  <- modelConststr(model, gram, freq)
    }
    return ( model )
}
