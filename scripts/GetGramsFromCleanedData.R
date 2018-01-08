 library(methods); library(compiler); library(fastmatch); library(NLP); library(dplyr);
source("scripts/ReadLines2.R")
source("scripts/RPointer.R")


SplitByNA <- function( x ){ #http://r.789695.n4.nabble.com/Split-a-vector-by-NA-s-is-there-a-better-solution-then-a-loop-td2075110.html
    idx <- 1 + cumsum( is.na( x ) )
    not.na <- ! is.na( x )
    split( x[not.na], idx[not.na] )
}

GetGramsFromFile <- function(fileName, termsToIncludePtr, gramSizesPtr, returnTermRef = FALSE) {
    lines = GetLinesFromFile(fileName)
    gramsHash <- hash()
    grams = lapply(lines, function(line) {
        rawGrams = strsplit(line, " ", fixed = T)[[1]] %>%
            fmatch(termsToIncludePtr$value) %>% # match references to a list of included terms
            SplitByNA() %>%
            lapply(function(x) { # generate grams as a int vector or the correspoding string ref
                if(!returnTermRef)
                    x = termsToIncludePtr$value[x]
                ngrams(x, gramSizesPtr$value)
            } )
        if(length(rawGrams) > 0) #combine lists of grams if any were found (to avoid error from passing empty object to combine
           rawGrams = combine(rawGrams)
        rawGrams = vapply(rawGrams, function(x) { # turn grams back to string
            if(!returnTermRef)
                paste(x,collapse = ' ')
            else
                as.character(x) %>%
                paste(collapse = ' ')
            }, "") %>%
            factor()
        rawGrams = tapply(rep.int(1,length(rawGrams)), rawGrams, sum)
        #hash(names(rawGrams),as.vector(rawGrams))
        values(gramsHash,keys=names(rawGrams)) <- values(gramsHash,names(rawGrams)) + as.vector(rawGrams)

     })
    #tapply(unlist(grams), names(unlist(grams)), sum)
    return(gramsHash)
}


GetGramsFromTrainingFiles <- function(inPath, termsToInclude, nPartitionsToUse, gramSizes = 2:3, filepattern = ".*\\.txt")
{
    # # gramsHashSet = hash()
    # values(h,keys=keys(h2)) <- values(h,keys(h2)) + values(h2)
    #

    filePaths = list.files(inPath, filepattern, full.names = TRUE)
    filesToUse = as.integer(gsub(".*part([0-9]+).*", "\\1", filePaths)) <= nPartitionsToUse
    filePaths = filePaths[filesToUse]

    gramsHashSet = hash()
    grams = lapply(filePaths, function(x) {
        h = GetGramsFromFile(x, newPointer(termsToInclude), newPointer(gramSizes))
        values(gramsHashSet,keys=keys(h)) <- values(gramsHashSet,keys(h)) + values(h)
        })
    return(gramsHashSet)

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



