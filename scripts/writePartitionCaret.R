library(caret)
dataPath <- "data/en_US/"
readWriteSubset <- compiler::cmpfun( function(name, filePath, NLines, NChars, xChars = NChars) {
	
    fileRead <- file(filePath, "rt")
    fileTrain <- file(paste0(dataPath, xChars,"/", name, xChars, "Train"), "wt")
    halves <- c(rep(1,NLines / 2),rep(2,NLines / 2)) # grab equal number from each half of file
    partition <- caret::createDataPartition(halves, p = xChars / NChars)
    i = 1
    j = 1
    nChars = 0
    nLines = 0
    while(length(line <- readLines(fileRead, n = 1L, warn=FALSE)) > 0) {
        if( j > length(partition[[1]]) ){
            break
        } else if( partition[[1]][j] == i) {
            write(line, fileTrain)
            nChars = nChars + nchar(line)
		    nLines = nLines + 1
		    j = j + 1
        }
        i = i + 1
    }
    close(fileRead)
    close(fileTrain)
   
    print(paste("Wrote", nChars, "chars", nLines, "lines from", name))
} )
twitFile = paste0(dataPath, "en_US.twitter.txt")
blogFile = paste0(dataPath, "en_US.blogs.txt")
newsFile = paste0(dataPath, "en_US.news.txt")
twitLines = 2360148
blogLines = 899288
newsLines = 77259
twitChars = 162384825
blogChars = 370746263
newsChars = 386430028
set.seed(353)
readWriteSubset("twitter", twitFile, twitLines, twitChars , xChars)
readWriteSubset("blog", blogFile, blogLines, blogChars , xChars)
readWriteSubset("news", newsFile, newsLines, newsChars , xChars)



