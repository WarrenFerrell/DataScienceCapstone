source("scripts/Utilities/ReadLines2.R")

WriteSubset <- function(inName, inFilePath, outPath, nPartitions = 2)
{
    lines <- ReadLines2(inFilePath)

    outBuffers <- split(lines, seq_along(lines) %% nPartitions)

    outFilePaths <- vapply(1:nPartitions, function(i)
        paste0(outPath, "part", i, inName), "")

    mapply(function(path, buf) {
        fp = file(path, "wt")
        write(unlist(buf, use.names = FALSE), fp, sep = '\n')
        close(fp)
    }, outFilePaths, outBuffers)

    return(outFilePaths)
}

WriteSubsetDriver <- function(inPath, outPath, nPartitions = 100, pattern = ".*\\.txt")
{
    dir.create(file.path(outPath), showWarnings = FALSE)

    fileNames <- list.files(inPath, pattern)
    filePaths <- list.files(inPath, pattern, full.names = TRUE)
    outFilePaths <- mapply(function(x,y)
    {
        WriteSubset(x, y, outPath, nPartitions)
    }
    , fileNames, filePaths)

    return(paste("Wrote", nPartitions, " to ", outPath))
}
