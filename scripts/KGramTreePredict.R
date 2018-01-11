source("scripts/Utilities/RPointer.R")


StupidBackOffPredictDriver <- function( prefix, gramTree, gramSizes, nResults)
{
    prefixLemmas =  strsplit(tolower(prefix), ' ')[[1]]
    maxGram = max(gramSizes)
    if(length(prefixLemmas) > maxGram - 1)
        prefixLemmas = prefixLemmas[(length(prefixLemmas) - maxGram+2):length(prefixLemmas)]
    prediction = StupidBackOffPredict(gramTree, prefixLemmas, gramTree$freq)

    return(sort(prediction, decreasing =T)[1:nResults])
}

StupidBackOffPredict <- function( gramTree, lemmas, freqOfParent = 1)
{
    matchTree = gramTree$children[[lemmas[[1]]]]
    pred = vector()
    if(length(lemmas) == 1){ #down to last lemma and must predict
        if(is.null(matchTree[[1]]))
            pred = setNames(vapply(gramTree$children,`[[`,1,'freq') / freqOfParent
                              , names(gramTree$children))
        else
            pred = setNames(vapply(matchTree$children,`[[`,1,'freq') / freqOfParent
                            , names(matchTree$children))
    } else {

        if(is.null(matchTree[[1]]))
            pred = c(pred, StupidBackOffPredict(gramTree, lemmas[2:length(lemmas)], freqOfParent) )
        else
            pred = c(pred, StupidBackOffPredict(matchTree, lemmas[2:length(lemmas)], gramTree$freq) )
    }

    return( pred )
}

# kGrams = lapply(names(grams), function(grams) strsplit(grams, ' ')[[1]])
# firstGramsFactor = factor(vapply(kGrams, `[[`, '', 1))
# lemmaToPredict = c('a', 'few')
# prediction = StupidBackOffPredict(kGramsTree, lemmaToPredict, kGramsTree$freq)
# print(sort(prediction, decreasing=T)[1:nResults])





