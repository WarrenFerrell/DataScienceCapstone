library(methods); library(hash); library(fastmatch); library(NLP); library(dplyr);
library(foreach);
source("scripts/Utilities/ReadLines2.R")
source("scripts/Utilities/RPointer.R")


SplitByNA <- function( x ){ #http://r.789695.n4.nabble.com/Split-a-vector-by-NA-s-is-there-a-better-solution-then-a-loop-td2075110.html
    idx <- 1 + cumsum( is.na( x ) )
    not.na <- ! is.na( x )
    split( x[not.na], idx[not.na] )
}

#https://stackoverflow.com/questions/30542128/circular-shifting-arrays-in-r-by-distance-n
ShiftVector <- function(x, n = 1) {
    if (n == 0) x else c(tail(x, -n), head(x, n))
}

GetGramsFromFileWithoutRepeats <- function(fileName, termsToIncludePtr, gramSizesPtr) {
    lines = GetLinesFromFile(fileName)
    grams = lapply(lines, function(line) {
        rawTerms = strsplit(line, " ", fixed = T)[[1]]
        termRefs = fmatch(rawTerms, termsToIncludePtr$value) # match references to a list of included terms
        termsSplit = SplitByNA(termRefs)
        rawGrams = lapply(termsSplit, function(t) { # generate grams as a int vector or the correspoding string ref
            t = mapply(function(x,xp1)
            {
                if(x == xp1)
                    NA
                else
                    x
            }, t, ShiftVector(t, 1))
            t = t[!is.na(t)]
            ngrams(t, gramSizesPtr$value)
        })
        if(!is.null(unlist(rawGrams))) #combine lists of grams if any were found (to avoid error from passing empty object
        {
            # turn grams back to string
            rawGrams = combine(rawGrams)
            vapply(rawGrams, paste, collapse = ' ', "")
        }
    })
    combine(grams)
}

GetGramsFromFileWithoutRepeatsFast <- function(fileName, termsToIncludePtr, gramSizesPtr) {
    lines = GetLinesFromFile(fileName)
    grams = lapply(lines, function(line) {
        rawGrams = strsplit(line, " ", fixed = T)[[1]]
        rawGrams = fmatch(rawGrams, termsToIncludePtr$value) # match references to a list of included terms
        rawGrams = SplitByNA(rawGrams)
        rawGrams = lapply(rawGrams, function(t) { # generate grams as a int vector or the correspoding string ref
            t = mapply(function(x,xp1)
            {
                if(x == xp1)
                    NA
                else
                    x
            }, t, ShiftVector(t, 1))
            t = t[!is.na(t)]
            ngrams(t, gramSizesPtr$value)
        })
        if(!is.null(unlist(rawGrams))) #combine lists of grams if any were found (to avoid error from passing empty object
        {
            # turn grams back to string
            rawGrams = combine(rawGrams)
            vapply(rawGrams, paste, collapse = ' ', "")
        }
    })
    combine(grams)
}


GetGramsFromFilesWithoutRepeatsDriver <- function(inPath, termsToInclude, nPartitionsToUse, gramSizes = 2:4, minFreq = 2, returnTermRef = FALSE, filepattern = '.*\\.txt')
{

    filePaths = list.files(inPath, filepattern, full.names = TRUE)
    filesToUse = as.integer(gsub(".*part([0-9]+).*", "\\1", filePaths)) <= nPartitionsToUse
    filePaths = filePaths[filesToUse]

    grams = lapply(filePaths, function(x) {
        GetGramsFromFileWithoutRepeatsFast(x, newPointer(termsToInclude), newPointer(gramSizes))
    })
    grams = combine(grams)
    grams = tapply(rep.int(1,length(grams)), grams, sum)
    grams = grams[grams >= minFreq]
    if(!returnTermRef)
    {
        termsToIncludePtr = newPointer(termsToInclude)
        gramsAsWords = vapply(names(grams), function(gram){
            gramRefs = as.integer(unlist(strsplit(gram, ' ', fixed=T), use.names = F))
            gramAsWords = vapply(gramRefs, function(gr) termsToIncludePtr$value[gr], '', USE.NAMES = FALSE)
            paste(gramAsWords,collapse = ' ')
        }, '', USE.NAMES = FALSE)
        return( setNames(unlist(grams, use.names = F), gramsAsWords) )
    }
    else
        return(grams)
}

# if( parallel ) {
#     corpus.all <- foreach(fileName = file.names) %dopar%  {
#         cleanFile(fileName) }
# } else {
#     corpus.all <- foreach(fileName = file.names) %do%  {
#         cleanFile(fileName) }
# }