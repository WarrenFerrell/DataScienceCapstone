library(foreach); library(pryr); library(compiler); library(fastmatch)


#Uses side effects to remove all terms with a frequency less than minFreq
cleanTree <- function(tree, minFreq) {
    if( !is.environment(tree) ) { #TRUE = called with 'gfreq' value
        return( tree > minFreq ) #calling recursion needs to rm ptr
    } else {
        keep <- FALSE
        for( term in ls(tree) ){
            if(cleanTree(tree[[term]], minFreq)) {
                keep <- keep + if(term == 'gfreq') 1 else 2.75
            } else {
                rm(term, pos = tree)
            }
        }
        return( keep )
    }
}

treeConststr <- function(tree, gram, sizePtr) {
    #browser()
    term <- as.character(gram[[1]])

    if( is.null(tree[[term]]) ) {
        tree[[term]] <-  new.env()
        sizePtr[['*']] <- sizePtr[['*']] + (160 * 2.75) # need to keep track of tree size
    }
    branch <- tree[[term]] #can modify the branch because environments have side effects
    if( is.null(branch[['gfreq']]) ) {
        branch[['gfreq']] <- 1
        sizePtr[['*']] <- sizePtr[['*']] + 160
    } else {
        branch[['gfreq']] <- 1 + branch[['gfreq']]
    }

    if( length(gram) > 1 )
        branch <- treeConststr(branch, gram[-1], sizePtr)
    return( tree )
}

#x should be an nGramTree
inPlaceTree.create <- function(x, wordRefs) {
    #browser()
    sizePtr <- new.env()
    sizePtr[['*']] <- x$size
    for(n in 1:(length(wordRefs)-1)) {
        if( !is.na(wordRefs[n]) ) { #ignore grams that start with NA
            gram <- if((length(wordRefs) - n + 1) >= x$maxGram )
                        wordRefs[n:(n + x$maxGram)]
                    else
                        wordRefs[n:length(wordRefs)] #save some chopping computation
            while(is.na(tail(gram,1)))
                gram <- gram[-length(gram)] #chop off ending NAs... not optimized
            x$tree <- treeConststr(x$tree, gram, sizePtr)
            rm(gram)
        }
    }
    x$size <- sizePtr[['*']]
    if( x$size >  x$maxSize ) {
        x$size <- cleanTree(x$tree, minFreq) * 160
        x$timesCleaned <- x$timesCleaned + 1
    }
    #x$tree <- tree     #not needed due to side effects with environments
    return( x )
}



readRefData <- function(filePath, commonTerms,
                        maxGram=10, maxSize, minFreq,
                        parallel=FALSE) {

    treeFromFile <- function(fileName) {
        fileCon <- file(fileName, "rt")
        gramTree <- nGramTree$new(maxGram = maxGram, maxSize = maxSize * MB,
                                minFreq = minFreq)
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- fmatch(words, commonTerms) # get word references
            if( length(wordRefs) > 1 )
                gramTree <- inPlaceTree.create(gramTree, wordRefs)
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
    trees <- list()
    for(i in seq_along(file.names)) {
        trees[i] <- treeFromFile(file.names[i])
        print(trees[i])
        #tree.all <- mergeTrees(tree.all, trees[[i]], TRUE, TRUE)
        #browser()
    }
    # formals(mergeTrees)$sideEffects <- TRUE
    # formals(mergeTrees)$removeOld <-  TRUE
    # setMethod('c', 'nGramTree', function(x, ...) (mergeTrees(unlist(x),...)))
    # formals(foreach)$.combine <- c
    # formals(foreach)$.init <- tree.all
    #
    # formals(foreach)$.multicombine <- FALSE
    # formals(foreach)$.packages <- c('fastmatch')
    # formals(foreach)$.inorder <- FALSE
    # formals(foreach)$.verbose <- TRUE
    # source('C:/Users/warre/Dropbox/GitHub/CAPSTONE/scripts/nGramTreeClass.R')
    # print(formals(foreach))
    # treeFromFile <- compiler::cmpfun( treeFromFile )
    # if( parallel ) {
    #     tree.all <- foreach(fileName = file.names) %dopar%  {
    #         treeFromFile(fileName) }
    # } else {
    #     tree.all <- foreach(fileName = file.names) %do%  {
    #         treeFromFile(fileName) }
    # }

    return( trees )

}
