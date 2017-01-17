library(foreach); library(pryr); library(compiler); library(fastmatch)


#Uses side effects to remove all terms with a frequency less than minFreq
cleanModel <- function(model, minFreq) {
    if( !is.environment(model) ) { #TRUE = called with 'gfreq' value
        return( model > minFreq ) #calling recursion needs to rm ptr
    } else {
        keep <- FALSE
        for( term in ls(model) ){
            if(cleanModel(model[[term]], minFreq)) {
                keep <- keep + if(term == 'gfreq') 1 else 2.75
            } else {
                rm(term, pos = model)
            }
        }
        return( keep )
    }
}

modelConststr <- function(model, gram, sizePtr) {
    #browser()
    term <- as.character(gram[[1]])

    if( is.null(model[[term]]) ) {
        model[[term]] <-  new.env()
        sizePtr[['*']] <- sizePtr[['*']] + (160 * 2.75) # need to keep track of model size
    }
    branch <- model[[term]] #can modify the branch because environments have side effects
    if( is.null(branch[['gfreq']]) ) {
        branch[['gfreq']] <- 1
        sizePtr[['*']] <- sizePtr[['*']] + 160
    } else {
        branch[['gfreq']] <- 1 + branch[['gfreq']]
    }

    if( length(gram) > 1 )
        branch <- modelConststr(branch, gram[-1], sizePtr)
    return( model )
}

#x should be an nGramModel
inPlaceModel.create <- function(x, wordRefs) {
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
            x$model <- modelConststr(x$model, gram, sizePtr)
            rm(gram)
        }
    }
    x$size <- sizePtr[['*']]
    if( x$size >  x$maxSize ) {
        x$size <- cleanModel(x$model, minFreq) * 160
        x$timesCleaned <- x$timesCleaned + 1
    }
    #x$model <- model     #not needed due to side effects with environments
    return( x )
}



readRefData <- function(filePath, commonTerms,
                        maxGram=10, maxSize, minFreq,
                        parallel=FALSE) {

    modelFromFile <- function(fileName) {
        fileCon <- file(fileName, "rt")
        gramModel <- nGramModel$new(maxGram = maxGram, maxSize = maxSize * MB,
                                minFreq = minFreq)
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- fmatch(words, commonTerms) # get word references
            if( length(wordRefs) > 1 )
                gramModel <- inPlaceModel.create(gramModel, wordRefs)
        }
        close(fileCon)
        return( gramModel )
    }

    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }
    model.all <- nGramModel()
    models <- list()
    for(i in seq_along(file.names)) {
        models[i] <- modelFromFile(file.names[i])
        print(models[i])
        #model.all <- mergeModels(model.all, models[[i]], TRUE, TRUE)
        #browser()
    }
    # formals(mergeModels)$sideEffects <- TRUE
    # formals(mergeModels)$removeOld <-  TRUE
    # setMethod('c', 'nGramModel', function(x, ...) (mergeModels(unlist(x),...)))
    # formals(foreach)$.combine <- c
    # formals(foreach)$.init <- model.all
    #
    # formals(foreach)$.multicombine <- FALSE
    # formals(foreach)$.packages <- c('fastmatch')
    # formals(foreach)$.inorder <- FALSE
    # formals(foreach)$.verbose <- TRUE
    # source('C:/Users/warre/Dropbox/GitHub/CAPSTONE/scripts/nGramModelClass.R')
    # print(formals(foreach))
    # modelFromFile <- compiler::cmpfun( modelFromFile )
    # if( parallel ) {
    #     model.all <- foreach(fileName = file.names) %dopar%  {
    #         modelFromFile(fileName) }
    # } else {
    #     model.all <- foreach(fileName = file.names) %do%  {
    #         modelFromFile(fileName) }
    # }

    return( models )

}
