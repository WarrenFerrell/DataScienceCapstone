library(methods)

if( !exists('MB') ) {
    MB <- 1048576
    lockBinding('MB', globalenv())
}

nGramTree <- setRefClass('nGramTree',
    fields = list(tree = 'environment', nVocab = 'numeric',
                  cleanFreq = 'numeric', nTopGrams = 'numeric',
                  maxGram = 'numeric', timesCleaned = 'numeric',
                  nCharInput = 'numeric'),
    method = list(
        initialize = function(..., tree = new.env(), nVocab = 2E4,
                 cleanFreq = 10000, nTopGrams = 10,
                 maxGram = 10, timesCleaned = 0) {
            callSuper(...,tree = tree, size = size,
                      cleanFreq = cleanFreq, nTopGrams = nTopGrams,
                      maxGram = maxGram, timesCleaned = timesCleaned)
         }
        )
)
#setMethod('c', 'nGramTree', function(x, ...) (mergeTrees(unlist(x),...)))
#setMethod('object.size', 'nGramTree', function(x) (x$size))

g1 <- nGramTree$new()
