library(foreach); library(tm); library(compiler); library(fastmatch)



readLineData <- function(filePath, commonTerms, xChars ) {
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
    corpus.all <- foreach(fileName = file.names, .combine = 'c',
                          .packages='tm') %dopar%  {
        fileCon <- file(fileName, "rt")
        lines <- list()

        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
			words <- strsplit(cleanLine(line), " ")[[1]] # split to words / punctuation
			words <- words[ words != "" ]
			fixedWords <- list()
			for( j in seq_along(words) ) {
				word <- words[[j]]
				if( word %in% commonTerms ) {
					fixedWords <- list(fixedWords, list(word))
				} else {
					fixedWords <- list(fixedWords, list("#")) # replace uncommon terms with a pound sign
				}
			}
			if( length(fixedWords) > 1 )
				lines <- list(lines, list(paste(unlist(fixedWords), collapse = " " ))) #merge
        }
        close(fileCon)
		return( tm::VCorpus(tm::VectorSource(unlist(lines))) )
    }

    return( corpus.all )
}
