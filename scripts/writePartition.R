dataPath <- "data/en_US/"
twitFile = paste0(dataPath, "en_US.twitter.txt")
blogFile = paste0(dataPath, "en_US.blogs.txt")
newsFile = paste0(dataPath, "en_US.news.txt")
twitChars = 162384825
blogChars = 370746263
newsChars = 386430028
readWriteSubset <- function(filePath, NChars, xChars = NChars) {
    fileRead <- file(filePath, "rt")
    fileWrite <- file(paste0(filePath, xChars, "partition"), "wt")
    p = xChars / NChars #probability that a line should be grabbed
    nLines = 0
    nChars = 0
    while(length(line <- readLines(fileRead, n = 1L, warn=FALSE)) > 0) {
		if( runif(1) < p ) {
			nLines = nLines + 1
			nChars = nChars + nchar(line)
			write(line, fileWrite)
		}
    }
    close(fileRead)
    close(fileWrite)
    name <- gsub(filePath, pattern = paste0(dataPath,"([[:alpha:]]+)/.*"), replace = "\\1")
    print(paste("Wrote", nChars, "chars", nLines, "lines from", name))
}

xChars = 5e7 #get 50MB of data from each
set.seed(354)
readWriteSubset(twitFile, twitChars , xChars)
readWriteSubset(blogFile, blogChars , xChars)
readWriteSubset(newsFile, newsChars , xChars)



