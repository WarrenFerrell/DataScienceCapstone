

gramOrder <-  function(gramFreq) { # order by gram length then frequency
    gramFreq <- gramFreq[order(gramFreq, decreasing = TRUE)]
    gramLength <- list()
    for( i in seq_along(gramFreq) ) {
        gram <- eval( parse(text = names(gramFreq[i])) )
        gramLength <- list( gramLength, list(length(gram)) )
    }
    return( gramFreq[order(unlist(gramLength))] )
}

treeConststr <- function(tree, gram, freq) {
    #browser()
    term = gram[[1]]
    if( !exists(term, where = tree) )
        tree[[term]] <- new.env()

    if( length(gram) == 1 ) {
        tree[[term]][['gfreq']] <- freq

        return( tree )
    } else {
        tree[[term]] <- treeConststr(tree[[term]], gram[-1], freq)
        return( tree )
    }
}

nGramTree.create <- function(gramFreq, depth = 12) {
    #browser()
    tree <- structure(new.env(), class = 'nGramTree')
    gramFreq <- gramOrder(gramFreq)
    for( i in seq_along(gramFreq) ) {
        gram <- eval(parse(text = names(gramFreq[i]))) %>% as.character
        freq <- gramFreq[[i]]
        tree  <- treeConststr(tree, gram, freq)
    }
    return ( tree )
}
