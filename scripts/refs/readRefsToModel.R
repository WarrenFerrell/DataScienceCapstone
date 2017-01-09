library(foreach); library(tm); library(compiler); library(fastmatch)



readRefData <- function(filePath, commonTerms, xChars ) {
    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }
    commonTerms <- c(commonTerms, "\"", ".", "?", "!", ",",";", ":", "&") # add punctuation to terms to keep
    cleanLine <- compiler::cmpfun( function(s){
        s <- gsub("DEL", "", iconv(s, "UTF-8", "ASCII", sub="DEL")) # remove non ASCII characters
        s <- tolower(s)
        gsub("([-'a-z]+)" , " \\1 ", s)  # place spaces before and after words
    } )

    # corpus.all <- VCorpus(VectorSource(""))
    # for(i in seq_along(file.names)) {
    #     corpus.all <- c(pruneData(file.names[i]), corpus.all)
    # }

    corpus.all <- foreach(fileName = file.names, .combine = 'c',
                          .packages=c('tm','fastmatch')) %dopar%  {
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
    }

    return( corpus.all )
}
