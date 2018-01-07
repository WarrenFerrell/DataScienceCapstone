
dataPath <- "data/en_US/"
mainPath <- "C:/Users/warre/GitHub/CAPSTONE/"

source(paste0(mainPath, "scripts/cleanLine.R"))

readWriteCleanedFile <- compiler::cmpfun( function(name, filePath) {

    fileRead <- file(filePath, "rt")
    fileTrain <- file(paste0(dataPath,"/ASCII", name, ".txt"), encoding = "ASCII", "wt")
    while(length(line <- readLines(fileRead, n = 1L, warn=FALSE)) > 0) {
        write(CleanLine(line), fileTrain)
    }
    close(fileRead)
    close(fileTrain)

} )
twitFile = paste0(dataPath, "en_US.twitter.txt")
blogFile = paste0(dataPath, "en_US.blogs.txt")
newsFile = paste0(dataPath, "en_US.news.txt")

readWriteCleanedFile("twitter", twitFile)
readWriteCleanedFile("blog", blogFile)
readWriteCleanedFile("news", newsFile)

