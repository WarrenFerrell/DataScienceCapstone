library(foreach); library(tm); library(compiler); library(fastmatch)

modelConststr <- function(model, gram) {
    #browser()
    term <- as.character(gram[[1]])
    if( !exists(term, where = model) )
        model[[term]] <-  new.env()
    branch <- model[[term]] #can modify the branch because environments have side effects
    branch[['gfreq']] <- 1 + if(exists('gfreq',branch)) branch[['gfreq']]
                             else 0
    if( length(gram) > 1 )
        branch <- modelConststr(branch, gram[-1])
    return( model )
}


inPlaceModel.create <- function(model, wordRefs, maxGram = 10) {
    #browser()

    for(n in 1:(length(wordRefs)-1)) {
        if( !is.na(wordRefs[n]) ) { #ignore grams that start with NA
            gram <- if(length(wordRefs) - n + 1 >= maxGram )
                        wordRefs[(n):(n + maxGram)]
                    else
                        wordRefs[(n):length(wordRefs)] #save some chopping computation
            while(is.na(tail(gram,1)))
                gram <- gram[-length(gram)] #chop off ending NAs... not optimized
            model <- modelConststr(model, gram)
        }
    }
    return( model )
}


readRefData <- function(filePath, commonTerms, maxGram = 10, parallel = FALSE ) {
    modelFromFile <- function(fileName) {
        fileCon <- file(fileName, "rt")
        model <- new.env()
        grams <- list()
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- fmatch(words, commonTerms) # get word references
            if( length(wordRefs) > 1 )
                model <- inPlaceModel.create(model, wordRefs, maxGram)
        }
        close(fileCon)
        return(model)
    }

    formals(foreach)$.combine <- mergeModels
    formals(foreach)$.packages <- c('fastmatch','NLP')
    formals(foreach)$.inorder <- FALSE
    formals(foreach)$.verbose <- TRUE

    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }

    model.all <- new.env()
    for(i in seq_along(file.names)) {
        model.all <- mergeModels(model.all, modelFromFile(file.names[i]))
    }

    # if( parallel ) {
    #     model.all <- foreach(fileName = file.names) %dopar%  {
    #         modelFromFile(fileName) }
    # } else {
    #     model.all <- foreach(fileName = file.names) %do%  {
    #         modelFromFile(fileName) }
    # }

    return( model.all )
}
