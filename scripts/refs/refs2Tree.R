 library(methods); library(compiler); library(fastmatch)
source("scripts/ReadLines2.R")

#Uses side effects to remove all terms with a frequency less than minFreq
cleanTreeByFreq <- function(tree, minFreq)
{
    nRemove <- 0
    for( term in ls(tree, sorted = FALSE) ){
        if(term != '#') {
            nRemove <- nRemove + cleanTreeByFreq(tree[[term]], minFreq)
            freq <- tree[[term]][['#']]
            if(!is.null(freq)) {
                if(freq < minFreq) {
                    rm('#', pos = tree[[term]])
                    nRemove <- nRemove + 1
                }
            }
            if( length(tree[[term]]) == 0 )
                rm(term, pos = tree)
        }
    }
    return( nRemove )
}

cleanTreeByTopGrams <- function(tree, nTopGrams)
{
    termList <- ls(tree, sorted = FALSE)
    nRemove <- 0
    termFreq <- vector('numeric')
    for( term in termList ){
        if( term != '#' ) {
            freq <-  tree[[term]][['#']]
            if(!is.null(freq))
                termFreq[term] <- freq
            nRemove <- nRemove + cleanTreeByTopGrams(tree[[term]], nTopGrams)
        }
    }
    termFreq <- sort.int(termFreq, method = 'radix')
    if( length(termFreq) > nTopGrams ) {
        for( term in names(termFreq[(nTopGrams+1):length(termFreq)]) ) {
            rm('#', pos = tree[[term]])
            nRemove <- nRemove + 1
            if( length(tree[[term]]) == 0 ) {
                rm(term, pos = tree)
            }
        }
    }
    return( nRemove )
}

cleanTreeByBoth <- function(tree, minFreq, nTopGrams)
{
    termList <- ls(tree, sorted = FALSE)
    nRemove <- 0
    termFreq <- vector('numeric')
    for( term in termList ){
        if( term != '#' ) {
            freq <-  tree[[term]][['#']]
            if(!is.null(freq)){
                if(freq < minFreq) {
                    rm('#', pos = tree[[term]])
                    nRemove <- nRemove + 1
                } else
                termFreq[term] <- freq
            }
            nRemove <- nRemove + cleanTreeByTopGrams(tree[[term]], nTopGrams)
        }
    }
    termFreq <- sort.int(termFreq, method = 'radix')
    if( length(termFreq) > nTopGrams ) {
        for( term in names(termFreq[(nTopGrams+1):length(termFreq)]) ) {
            rm('#', pos = tree[[term]])
            nRemove <- nRemove + 1
            if( length(tree[[term]]) == 0 ) {
                rm(term, pos = tree)
            }
        }
    }
    return( nRemove )
}

treeConststr <- function(tree, gram)
{
    #browser()
    term <- as.character(gram[[1]])
    if( is.null(tree[['#']]) )
        tree[['#']] <- 1
    else
        tree[['#']] <- 1 + tree[['#']]
    if( is.null(tree[[term]]) )
        tree[[term]] <-  new.env()
    if( length(gram) > 1 )
        tree[[term]] <- treeConststr(tree[[term]], gram[-1])
    return( tree )
}

inPlaceTree <- function(tree, wordRefs, cleanFreq, nTopGrams, maxGram)
{
    #browser()
    NWords <- length(wordRefs)
    for(n in 1:(NWords-1)) {
        tree <- treeConststr(tree, if((length(wordRefs) - n + 1) >= maxGram )
                                    wordRefs[n:(n + maxGram)]
                                else
                                    wordRefs[n:NWords]
                               )
    }
    return( tree )
}


GetGramsFromFile <- function(inPath, termsToInclude, nPartitionsToUse, gramSizes = 2:3, filepattern = ".*\\.txt")
{
    gramsHashSet = hash()
    GramsFromFileClosure <- function(fileName) {
        lines = GetLinesFromFile(fileName)
        grams = lapply(lines, function(line) {
            line = strsplit(line, " ", fixed = T)[[1]] %>%
                fmatch(termsToInclude) %>% # match references to a list of included terms
                na.omit() %>% #remove unused terms
                ngrams(gramSizes)
                #as.character() %>%
                # tm::VectorSource() %>%
                # tm::VCorpus() %>%
                # tm::TermDocumentMatrix(x, list(global = list(c(minFreq,Inf))
            return(line)
            })
        return(grams)
    }

    btapply(unlist(L), names(unlist(L)), sum)


    filePaths = list.files(inPath, filepattern, full.names = TRUE)
    filesToUse = as.integer(gsub(".*part([0-9]+).*", "\\1", filePaths)) <= nPartitionsToUse
    filePaths = filePaths[filesToUse]
    return(lapply(filePaths, GramsFromFileClosure))
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



