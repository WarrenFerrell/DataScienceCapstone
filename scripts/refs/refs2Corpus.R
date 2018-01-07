library(foreach); library(tm); library(compiler); library(fastmatch)



readRefData <- function(filePath, commonTerms, xChars, parallel = FALSE ) {
    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }

    cleanFile <- compiler::cmpfun( function(fileName) {
        fileCon <- file(fileName, "rt")
        lines <- list()
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
            words <- words[ words != "" ]
            wordRefs <- as.character(fmatch(words, commonTerms)) #need to store as character so unlist doesn't break vectors
            if( length(wordRefs) > 1 )
                lines <- list(lines, list(paste(unlist(wordRefs), collapse = " " ))) #merge
        }
        close(fileCon)
        return( tm::VCorpus(tm::VectorSource(unlist(lines))) )
    } )

	formals(foreach)$.combine <- 'c'
	formals(foreach)$.packages <- c('tm','fastmatch')
	formals(foreach)$.inorder <- FALSE
	formals(foreach)$.verbose <- TRUE

    if( parallel ) {
        corpus.all <- foreach(fileName = file.names) %dopar%  {
            cleanFile(fileName) }
    } else {
        corpus.all <- foreach(fileName = file.names) %do%  {
            cleanFile(fileName) }
    }

    return( corpus.all )
}
