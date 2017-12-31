library(methods)

if( !exists('MB') ) {
    MB <- 1048576
    lockBinding('MB', globalenv())
}

nGramTree <- setRefClass('nGramTree',
    fields = list(tree = 'environment', nVocab = 'numeric', cleanFreq = 'numeric',
                  maxGram = 'numeric', nCharInput = 'numeric',
                  timesCleaned = 'numeric', termsRemoved = 'numeric',
                  cleanFunc = 'function', cleanLimit = 'numeric')
    # method = list(
    #     initialize = function(tree = new.env(), nVocab = 2E4,
    #              cleanFreq = 1E6, cleanFunc = length, cleanLimit = 2,
    #              maxGram = 10, nCharInput,
    #              timesCleaned = 0, termsRemoved = 0, ...) {
    #         callSuper(..., tree = tree, nVocab = nVocab,
    #                   cleanFreq = cleanFreq, cleanFunc = cleanFunc, cleanLimit = cleanLimit,
    #                   maxGram = maxGram, nCharInput = nCharInput,
    #                   timesCleaned = timesCleaned, termsRemoved = termsRemoved)
    #      } )
)
#setMethod('c', 'nGramTree', function(x, ...) (mergeTrees(unlist(x),...)))
#setMethod('object.size', 'nGramTree', function(x) (x$size))

g1 <- nGramTree$new()
