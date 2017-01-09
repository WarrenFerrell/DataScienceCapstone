readSentenceData <- function(filePath, commonTerms, xChars ) {
    file.names <- dir(filePath, full.names = TRUE)
    if( length( file.names ) != 3) {
        source("scripts/writePartitionCaret.R")
        file.names <- dir(filePath, full.names = TRUE)
    }
    for(i in 1:length(file.names) ){
        fileCon <- file(file.names[i], "rt")
        sentences <- list()
        while(length(line <- readLines(fileCon, n = 1L, warn=FALSE)) > 0) {
            
            # sentences <- list()
            # commonTerms <- c("\"", ".", "?", "!", ",","'",";", "berry", "oranges", "apples")
            # line <- "the, Apple's berry's..😃 a pear berry ? oRanges !.? cran-berry,Berries berry"
            
            line <- gsub(" ([.!?])[.!?]*" , "\\1\\|", cleanLine(line))
            phrases <- strsplit(line , split = "\\|") # split by sentences
            for(i in seq_along(phrases[[1]])){
                sentence <- phrases[[1]][[i]]        # examine each sentence separately
                if( nchar(sentence) > 3 ) {          # sentence must be at least 2 words i.e. "I am"   
                    words <- strsplit(sentence, " ")[[1]] # split to words / punctuation
                    words <- words[ words != "" ]
                    fixedWords <- c()
                    for( j in seq_along(words) ) {
                        word <- words[[j]]
                        if(nchar(word) > 0){
                            if( !(word %in% commonTerms) ) {
								fixedWords <- list(fixedWords, list("#"))   # replace uncommon terms with a pound sign
							} else {
								fixedWords <- list(fixedWords, list(word))
							}
                        }
                    }
                    if( length(fixedWords) > 1 )
                        sentences <- list(sentences, list(paste(unlist(fixedWords), collapse = " " ))) #merge 
                }
            }
        }
        close(fileCon)
    }
    corpus <- tm::VCorpus(VectorSource(unlist(sentences)),
        readerControl = list(reader = readPlain, language = "en_US"))
    return(corpus)
}
