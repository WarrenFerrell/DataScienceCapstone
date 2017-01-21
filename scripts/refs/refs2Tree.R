library(foreach); library(methods); library(compiler); library(fastmatch)


#Uses side effects to remove all terms with a frequency less than minFreq
cleanTreeByFreq <- function(tree, minFreq) {
    keep <- FALSE
    if( is.numeric(tree) ) { #TRUE = called with '#' value
        return( tree < minFreq ) #calling recursion needs to rm ptr
    } else {
        for( term in ls(tree, sorted = FALSE) ){
            if(cleanTreeByFreq(tree[[term]], minFreq)) {
                keep <- keep + 1
            } else {
                rm(term, pos = tree)
            }
        }
    }
    return( keep )
}

cleanTreeByTopGrams <- function(tree, nTopGrams) {
    termList <- ls(tree, sorted = FALSE)
    nKept <- length(termList)
    termFreq <- vector('numeric')
    for( term in termList ){
        if( term != '#' ) {
            if(!is.null(tree[[term]][['#']]))
                termFreq[term] <- tree[[term]][['#']]
            nKept <- nKept + cleanTreeByTopGrams(tree[[term]], nTopGrams)
        }
    }
    termFreq <- sort.int(termFreq, method = 'radix')
    if( length(termFreq) > nTopGrams ) {
        for( term in names(termFreq[(nTopGrams+1):length(termFreq)]) ) {
            rm('#', pos = tree[[term]])
            nKept <- nKept - 1
            if( length(ls(tree[[term]], sorted = FALSE)) == 0 )
                rm(term, pos = tree)

        }
    }
    return( nKept )
}

treeConststr <- function(tree, gram) {
    #browser()
    term <- as.character(gram[[1]])
    if( is.null(tree[['#']]) )
        tree[['#']] <- 1
    else
        tree[['#']] <- 1 + tree[['#']]
    if( is.null(tree[[term]]) )
        tree[[term]] <-  new.env()
    if( length(gram) > 1 )
        tree[[term]] <- treeConststr(tree[[term]], gram[-1])
    return( tree )
}

#x should be an nGramTree
inPlaceTree <- function(x, wordRefs, cleanFreq, nTopGrams, maxGram) {
    #browser()
    NWords <- length(wordRefs)
    for(n in 1:(NWords-1)) {
        x <- treeConststr(x, if((length(wordRefs) - n + 1) >= maxGram )
                                    wordRefs[n:(n + maxGram)]
                                else
                                    wordRefs[n:NWords]
                               )
    }
    return( x )
}


readRefData <- function(filePath, commonTerms, nVocab, cleanFreq, minFreq = 1,
                        nTopGrams = NULL, maxGram, xChars,
                        foreach = FALSE, parallel=FALSE) {
    inPlaceTree.close <- function(x, wordRefs) { #create closure (for parallel and visibility)
        inPlaceTree(x, wordRefs, cleanFreq, nTopGrams, maxGram)
    }
    treeFromFile <- function(fileName, gramTree) {
        fileCon <- file(fileName, "rt")
        timesCleaned <- 0
        termsKept <- 0
        nBytes <- 0
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- fmatch(words, commonTerms) # get word references
            i <- 1
            while( i < length(wordRefs) ) {
                k <- i
                while(!is.na(wordRefs[k])) #find NAs
                    k <- k + 1
                if( k > (i + 1) )
                    gramTree$tree <- inPlaceTree.close(gramTree$tree, wordRefs[i:(k-1)])
                i <- k + 1
            }
            if( (nBytes <- nBytes + nchar(line)) >= cleanFreq ) {
                timesCleaned <- timesCleaned + 1
                termsKept <- termsKept + gramTree$cleanFunc(gramTree$tree, gramTree$cleanLimit)
                nBytes <- 0
            }
        }
        close(fileCon)
        gramTree$timesCleaned <- timesCleaned
        gramTree$termsKept <- termsKept
        return( gramTree )
    }

    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }

    blankTree <- function() {
        tree <- nGramTree$new(tree = new.env(), nVocab = nVocab, cleanFreq = cleanFreq,
                              maxGram = maxGram, nCharInput =  xChars,
                              timesCleaned = 0, termsKept = 0)
        if(is.null(nTopGrams)) {
            tree$cleanFunc <- cleanTreeByFreq
            tree$cleanLimit <- minFreq
        } else {
            tree$cleanFunc <- cleanTreeByTopGrams
            tree$cleanLimit <- nTopGrams
        }
        return(tree)
    }
    tree.all <- blankTree()
    mergeTrees.close <- function(...) { mergeTrees(..., sideEffects = TRUE) }
    foreach.close <- function(...) {
        foreach(..., .multicombine = TRUE, .inorder = FALSE, .verbose = FALSE,
                .combine = mergeTrees.close)
    }
    if( !foreach ) {
        trees <- list()
        for(fName in file.names) {
            trees[[fName]] <- treeFromFile(fName, blankTree())
            #print(trees[fName])
            tree.all <- mergeTrees.close(tree.all, trees[[fName]])
        }
    } else {
        if( parallel ) {
            tree.all <- foreach.close(fileName = file.names,
                                      .export = c('cleanLine', 'inPlaceTree', 'treeConststr',
                                                  'cleanTree', 'treeFromFile', 'blankTree'),
                                      .packages = c('fastmatch')) %dopar%  {

                                          treeFromFile(fileName, blankTree()) }
        } else {
            tree.all <- foreach.close(fileName = file.names) %do%  {
                treeFromFile(fileName, blankTree()) }
        }
    }
    return( tree.all )
    #return( trees )


}
