

if( !exists('MB') ) {
    MB <- 1048576
    lockBinding('MB', globalenv())
}

nGramTree <- setRefClass('nGramTree',
    fields = list(tree = 'environment', size = 'numeric',
                  maxSize = 'numeric', minFreq = 'numeric',
                  maxGram = 'numeric', timesCleaned = 'numeric'),
    method = list(
        initialize = function(..., tree = new.env(), size = 2E5,
                              maxSize = 100 * MB, minFreq = 2,
                 maxGram = 10, timesCleaned = 0) {
            callSuper(...,tree = tree, size = size,
                      maxSize = maxSize, minFreq = minFreq,
                      maxGram = maxGram, timesCleaned = timesCleaned)
         }#, modSize = function(x) {
        #     size[['size']] <<- size[['size']] + x
        # }, getSize = function() {
        #     size[['size']]
        # }
        )
)
setMethod('c', 'nGramTree', function(x, ...) (mergeTrees(unlist(x),...)))
setMethod('object.size', 'nGramTree', function(x) (x$size))

g1 <- nGramTree$new()
