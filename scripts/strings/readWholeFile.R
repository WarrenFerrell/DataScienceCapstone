library(foreach); library(tm)

cleanLine <- compiler::cmpfun( function(s){
    s <- gsub("DEL", "", iconv(s, "UTF-8", "ASCII", sub="DEL")) # remove non ASCII characters
    s <- tolower(s)
    gsub("([-'a-z]+)" , " \\1 ", s)  # place spaces before and after words
} )

keepTerms <- function(s, terms) {
	lines <- strsplit(cleanLine(s), "\n")[[1]]
	fixedLines <- list()
	for( i in seq_along(lines) ) {
		words <- strsplit(lines[[i]], " ")[[1]] # split to words / punctuation
		words <- words[ words != "" ]
		fixedWords <- list()
		for( j in seq_along(words) ) {
			word <- words[[j]]
			if( word %in% terms ) {
				fixedWords <- list(fixedWords, list(word)) # appending lists to lists is fastest in R
			} else {
				fixedWords <- list(fixedWords, list("#")) # replace uncommon terms with a pound sign
			}
		}
		if( length(fixedWords) > 1 )
			fixedLines <- list(fixedLines,  list(paste(unlist(fixedWords), collapse = " " )) ) #merge
	}
	return( paste(unlist(fixedLines), collapse = "\n") )
}


readFileData <- function(filePath, commonTerms, xChars ) {
    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }

    formals(keepTerms)$terms <- commonTerms
    text <- foreach(fileName = file.names) %dopar%  {
        fileCon <- file(fileName, "rt")
        ret <- paste0(readLines(fileCon, warn=FALSE), collapse = "\n")
        close(fileCon)
        return( keepTerms(ret) )
    }

    return( tm::VCorpus(VectorSource(unlist(text))) )
}
