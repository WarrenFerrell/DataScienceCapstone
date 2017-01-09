library(tm) ; library(foreach); library(dplyr)
dataPath <- "data/en_US/"
if ( file.exists("objects/tdm.trim") ) {
    load(file = "objects/tdm.trim")
} else {
    corpora.raw <- tm::VCorpus(tm::DirSource( dataPath, recursive = TRUE))

    transf <- list( content_transformer(tolower),
                    removeNumbers,
                    removePunctuation,
                    stripWhitespace )
    corpora.trim <- tm_map(corpora.raw, tm_reduce, tmFuns = transf)
    rm(corpora.raw)
    save(corpora.trim, file = "objects/corpora.trim")
}

cntrl <- list(wordLengths = c(1, Inf), global = list(c(3,Inf))) #any word, min freq = 3
tdm.trim <- foreach(corpus = corpora.trim, .combine = c) %do%  {
    VCorpus(VectorSource(corpus)) %>%
        tm::TermDocumentMatrix( control = cntrl )
}
save(tdm.trim, file = "objects/tdm.trim")
