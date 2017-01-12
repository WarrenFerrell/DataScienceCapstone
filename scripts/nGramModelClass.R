library(pryr)

nGramModel <- function(model = new.env(), size = mem_used() * 0,
                       maxMem = size + 104857600, minFreq = 2,
                       maxGram = 10){
    structure(list(model, size, maxMem, minFreq),
              class = 'nGramModel')
}

object.size.nGramModel <- function(x) x$size