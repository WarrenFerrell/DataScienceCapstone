ReadLines2 <- function(fname) { #https://www.r-bloggers.com/faster-files-in-r/
    s = file.info( fname )$size
    buf = readChar( fname, s, useBytes=T)
    strsplit( buf,"(\r\n)|\n",useBytes=T)[[1]]
}

GetLinesFromFile <- function(fname) {
    filePtr = file(fname, open = "rt")
    buffer <- readLines( filePtr, warn=F)
    close(filePtr)
    return(buffer)
}