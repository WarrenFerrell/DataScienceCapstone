library(tm) ; library(foreach); library(dplyr)
dataPath <- "data/en_US/"
cleanLine <- compiler::cmpfun( function(s){
    s <- gsub("DEL", "", iconv(s, "UTF-8", "ASCII", sub="DEL")) # remove non ASCII characters
    s <- tolower(s)
    gsub("([-'a-z]+)" , " \\1 ", s)  # place spaces before and after words
} )
if ( file.exists("objects/copora.trim") ) {
    load(file = "objects/corpora.trim")
} else {
    corpora.raw <- tm::VCorpus(tm::DirSource( dataPath, pattern = "\\.txt$"))
    transf <- list( content_transformer(cleanLine),
                    removeNumbers,
                    removePunctuation,
                    stripWhitespace )
    corpora.trim <- tm_map(corpora.raw, tm_reduce, tmFuns = transf)
    rm(corpora.raw)
    save(corpora.trim, file = "objects/corpora.trim")
}
cntrl <- list(wordLengths = c(1, Inf),
              bounds = list(global = c(3,Inf))) #any word, min freq = 3
tdm.trim <- foreach(corpus = corpora.trim, .combine = 'c') %do%  {
    VCorpus(VectorSource(corpus)) %>%
        tm::TermDocumentMatrix( control = cntrl )
}
save(tdm.trim, file = "objects/tdm.trim")
