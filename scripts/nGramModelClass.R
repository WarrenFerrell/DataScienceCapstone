

if( !exists('MB') ) {
    MB <- 1048576
    lockBinding('MB', globalenv())
}

nGramModel <- setRefClass('nGramModel',
    fields = list(model = 'environment', size = 'numeric',
                  maxSize = 'numeric', minFreq = 'numeric',
                  maxGram = 'numeric', timesCleaned = 'numeric'),
    method = list(
        initialize = function(..., model = new.env(), size = 2E5,
                              maxSize = 100 * MB, minFreq = 2,
                 maxGram = 10, timesCleaned = 0) {
            callSuper(...,model = model, size = size,
                      maxSize = maxSize, minFreq = minFreq,
                      maxGram = maxGram, timesCleaned = timesCleaned)
         }#, modSize = function(x) {
        #     size[['size']] <<- size[['size']] + x
        # }, getSize = function() {
        #     size[['size']]
        # }
        )
)

setMethod('object.size', 'nGramModel', function(x) (x$size))

g1 <- nGramModel$new()
