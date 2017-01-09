library(foreach); library(tm); library(compiler); library(fastmatch)



readRefData <- function(filePath, commonTerms, xChars, parallel = FALSE ) {
    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }

    cleanLine <- compiler::cmpfun( function(s){
        s <- gsub("DEL", "", iconv(s, "UTF-8", "ASCII", sub="DEL")) # remove non ASCII characters
        s <- tolower(s)
        gsub("([-'a-z]+)" , " \\1 ", s)  # place spaces before and after words
    } )

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


    if( parallel ) {
        corpus.all <- foreach(fileName = file.names, .combine = 'c',
                              .packages=c('tm','fastmatch'),
                              .inorder=FALSE,
                              .verbose=TRUE) %dopar%  {
            cleanFile(fileName)
                              }
    } else {
        corpus.all <- foreach(fileName = file.names, .combine = 'c',
                              .packages=c('tm','fastmatch'),
                              .inorder=FALSE,
                              .verbose=TRUE) %do%  {
            cleanFile(fileName)
                              }
    }

    return( corpus.all )
}
