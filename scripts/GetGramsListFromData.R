library(methods); library(hash); library(fastmatch); library(NLP); library(dplyr);
library(foreach);
source("scripts/ReadLines2.R")
source("scripts/RPointer.R")


SplitByNA <- function( x ){ #http://r.789695.n4.nabble.com/Split-a-vector-by-NA-s-is-there-a-better-solution-then-a-loop-td2075110.html
    idx <- 1 + cumsum( is.na( x ) )
    not.na <- ! is.na( x )
    split( x[not.na], idx[not.na] )
}

GetGramsFromFile <- function(fileName, termsToIncludePtr, gramSizesPtr, returnTermRef = FALSE) {
    lines = GetLinesFromFile(fileName)
    grams = lapply(lines, function(line) {
        rawTerms = strsplit(line, " ", fixed = T)[[1]]
        termRefs = fmatch(rawTerms, termsToIncludePtr$value) # match references to a list of included terms
        termsSplit = SplitByNA(termRefs)
        rawGrams = lapply(termsSplit, function(x) { # generate grams as a int vector or the correspoding string ref
            if(!returnTermRef)
                x = termsToIncludePtr$value[x]
            ngrams(x, gramSizesPtr$value)
        })
        if(!is.null(unlist(rawGrams))) #combine lists of grams if any were found (to avoid error from passing empty object
        {
            rawGrams = combine(rawGrams)
            rawGrams = vapply(rawGrams, function(x) { # turn grams back to string
                if(!returnTermRef)
                    paste(x,collapse = ' ')
                else
                    as.character(x) %>%
                    paste(collapse = ' ')
            }, "") %>%
                factor()
            tapply(rep.int(1,length(rawGrams)), rawGrams, sum)

        }
    })
    tapply(unlist(grams), names(unlist(grams)), sum)
}


GetGramsListFromTrainingFiles <- function(inPath, termsToInclude, nPartitionsToUse, gramSizes = 2:3, filepattern = ".*\\.txt")
{

    filePaths = list.files(inPath, filepattern, full.names = TRUE)
    filesToUse = as.integer(gsub(".*part([0-9]+).*", "\\1", filePaths)) <= nPartitionsToUse
    filePaths = filePaths[filesToUse]

    grams = lapply(filePaths, function(x) {
        GetGramsFromFile(x, newPointer(termsToInclude), newPointer(gramSizes))
    })
    tapply(unlist(grams), names(unlist(grams)), sum)
    #tapply(unlist(grams), names(unlist(grams)), sum)

}

#
# ngram_tokenizer <- Token_Tokenizer(ngram_tokenizer)
# cntrl <- list(tokenizer = ngram_tokenizer, global = list(c(2,Inf)))
# gdm <- lapply(corpora, function(x) tm::TermDocumentMatrix(x, cntrl))
# gramFreq <- lapply(gdm, function(x) as.matrix(slam::rollup(x, 2, FUN = sum)))
# gramFreq <- lapply(gramFreq, function(x) x[order(x, decreasing = TRUE), ])
# for(fName in fileNames) {
#     trees[[fName]] <- treeFromFile(fName, blankTree())
#     #print(trees[fName])
#     fullTree <- mergeTreesClosure(fullTree, trees[[fName]])
# }
#
# if( parallel ) {
#     corpus.all <- foreach(fileName = file.names) %dopar%  {
#         cleanFile(fileName) }
# } else {
#     corpus.all <- foreach(fileName = file.names) %do%  {
#         cleanFile(fileName) }
# }



