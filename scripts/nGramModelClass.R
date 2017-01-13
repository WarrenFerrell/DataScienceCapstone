library(pryr)

nGramModel <- function(model = new.env(), size = mem_used() * 0,
                       maxMem = 100, minFreq = 2,
                       maxGram = 10, timesCleaned=0){
    attr <- list(model=model,size=size,maxMem=maxMem,
                 minFreq=minFreq,maxGram=maxGram,
                 timesCleaned=timesCleaned)
    structure(attr, class = 'nGramModel')
}

object.size.nGramModel <- function(x) x$size