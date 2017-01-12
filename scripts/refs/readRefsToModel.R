library(foreach); library(pryr); library(compiler); library(fastmatch)

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

#Uses side effects to remove all terms with a frequency less than minFreq
cleanModel <- function(model, minFreq) {
    if( !is.environment(model) ) { #TRUE = called with 'gfreq' value
        return( model < minFreq ) #calling recursion needs to rm ptr
    } else {
        delete <- TRUE
        for( term in ls(model) ){
            if(cleanModel(model[[term]], minFreq))
                delete <- FALSE
            else
                rm(term, pos = model)
        }
        return( delete )
    }
}

#

inPlaceModel.create <- function(gramModel, wordRefs, startMem) {
    #browser()
    model <- gramModel$model
    for(n in 1:(length(wordRefs)-1)) {
        if( !is.na(wordRefs[n]) ) { #ignore grams that start with NA
            gram <- if((length(wordRefs) - n + 1) >= gramModel$maxGram )
                        wordRefs[(n):(n + gramModel$maxGram)]
                    else
                        wordRefs[(n):length(wordRefs)] #save some chopping computation
            while(is.na(tail(gram,1)))
                gram <- gram[-length(gram)] #chop off ending NAs... not optimized
            model <- modelConststr(model, gram)
        }
    }
    if( (mem_used() - startMem) > (gramModel$maxMem * 1048576)){
        cleanModel(model, minFreq)
    }
    gramModel$size <- mem_used() - startMem
    #gramModel$model <- model     #not needed due to side effects with environments
    return( gramModel )
}


readRefData <- function(filePath, commonTerms,
                        maxGram=10, maxMem, minFreq,
                        parallel=FALSE) {

    modelFromFile <- function(fileName) {
        startMem <- pryr::mem_used()
        fileCon <- file(fileName, "rt")
        gramModel <- nGramModel(maxMem = maxMem, maxGram = maxGram,
                                minFreq = minFreq)
        grams <- list()
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- fmatch(words, commonTerms) # get word references
            if( length(wordRefs) > 1 )
                gramModel <- inPlaceModel.create(gramModel, wordRefs, startMem)
        }
        close(fileCon)
        return( gramModel )
    }

    formals(foreach)$.combine <- mergeModels
    formals(foreach)$.packages <- c('fastmatch','pryr')
    formals(foreach)$.inorder <- FALSE
    formals(foreach)$.verbose <- TRUE

    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }
    model.all <- nGramModel()
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
