source("scripts/Utilities/ReadLines2.R")


WriteGramsToFileByFreq <- function(grams , outPath, outName )
{
    dir.create(file.path(outPath), showWarnings = FALSE)

    outFile <- file(paste0(outPath, outName), encoding = "ASCII", "wt")
    grams = grams[ order(grams, decreasing = T, method='radix') ]
    mapply(function(gram, freq)
    {
        write(c(gram, freq), outFile, sep = ',', ncolumns = 2)
    }, names(grams), grams)

    close(outFile)
}

WriteGramsToFileByFirstGram <- function(grams , outPath, outName )
{
    dir.create(file.path(outPath), showWarnings = FALSE)

    outFile <- file(paste0(outPath, outName), encoding = "ASCII", "wt")

    firstgrams = vapply(names(grams), function(gram)
    {
        strsplit(gram, ' ')[[1]][[1]]
    },"")
    grams = grams[ order(firstgrams)]
    mapply(function(gram, freq)
    {
        write(c(gram, freq), outFile, sep = ',', ncolumns = 2)
    }, names(grams), grams)

    close(outFile)
}

ReadSavedGramsFromFile <- function( filePath )
{
    lines = GetLinesFromFile(filePath)
    splitOnGram = strsplit(lines, ',')
    grams = vapply(splitOnGram, `[[`, '', 1)
    freqs = vapply(splitOnGram, `[[`, '', 2)
    setNames(freqs, grams)
}