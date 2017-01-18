library(foreach); library(methods); library(compiler); library(fastmatch)


#Uses side effects to remove all terms with a frequency less than minFreq
cleanTree <- function(tree, minFreq) {
    if( !is.environment(tree) ) { #TRUE = called with '#' value
        return( tree > minFreq ) #calling recursion needs to rm ptr
    } else {
        keep <- FALSE
        for( term in ls(tree) ){
            if(cleanTree(tree[[term]], minFreq)) {
                keep <- keep + if(term == '#') 1 else 2.75
            } else {
                rm(term, pos = tree)
            }
        }
        return( keep )
    }
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
inPlaceTree.create <- function(x, wordRefs) {
    #browser()
    NWords <- length(wordRefs)
    for(n in 1:(NWords-1)) {
        x$tree <- treeConststr(x$tree, if((length(wordRefs) - n + 1) >= x$maxGram )
                                    wordRefs[n:(n + x$maxGram)]
                                else
                                    wordRefs[n:NWords]
                               )
    }
    return( x )
}


readRefData <- function(filePath, commonTerms, nVocab, cleanFreq,
                        nTopGrams, maxGram, nCharInput,
                        parallel=FALSE) {

    trees <- list()
    for(i in seq_along(file.names)) {
        trees[i] <- nGramTree$new(new.env(), nVocab, cleanFreq, nTopGrams,
                                  maxGram, nCharInput)

    }

    treeFromFile <- function(fileName) {
        fileCon <- file(fileName, "rt")
        gramTree <- nGramTree$new(maxGram = maxGram, maxSize = maxSize * MB,
                                minFreq = minFreq)
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
                    gramTree <- inPlaceTree.create(gramTree, wordRefs[i:(k-1)])
                i <- k + 1
            }

        }
        close(fileCon)
        return( gramTree )
    }

    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }
    tree.all <- nGramTree()

    # for(i in seq_along(file.names)) {
    #     trees[i] <- treeFromFile(file.names[i])
    #     print(trees[i])
    #     tree.all <- mergeTrees(tree.all, trees[[i]], sideEffects = TRUE, removeOld = FALSE)
    #     #browser()
    # }

    formals(mergeTrees)$sideEffects <-  TRUE

    formals(foreach)$.init <- tree.all
    formals(foreach)$.multicombine <- TRUE
    formals(foreach)$.packages <- c('fastmatch')
    formals(foreach)$.inorder <- FALSE
    #formals(foreach)$.verbose <- TRUE
    #print(formals(foreach))
    treeFromFile <- compiler::cmpfun( treeFromFile )
    if( parallel ) {

        tree.all <- foreach(fileName = file.names, .combine = mergeTrees) %dopar%  {
            treeFromFile(fileName) }
    } else {
        tree.all <- foreach(fileName = file.names, .combine = mergeTrees) %do%  {
            treeFromFile(fileName) }
    }

    return( tree.all )
    return( trees )

}
