source("scripts/cleanLine.R")
source("scripts/ReadLines2.R")

WriteCleanedFile <- function(inFilePath, outFilePath)
{
    lines <- GetLinesFromFile(inFilePath)
    lines <- vapply(lines, CleanLine, "")

    outFile <- file(outFilePath, encoding = "ASCII", "wt")
    write(lines, outFile, sep = '\n')
    close(outFile)
    return(outFilePath)
}
#WriteCleanedFile <-  compiler::cmpfun(WriteCleanedFile)

WriteASCIIDataDriver <- function(inPath , outPath, pattern = ".*\\.txt" )
{
    dir.create(file.path(outPath), showWarnings = FALSE)

    fileNames <- list.files(inPath, pattern)
    filePaths <- list.files(inPath, pattern, full.names = TRUE)
    mapply(function(x,y)
        {
            outFilePath <- paste0(outPath,"ASCII", x)
            WriteCleanedFile(y, outFilePath)
        }
        , fileNames, filePaths)
}

