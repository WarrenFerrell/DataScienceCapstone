source("scripts/Utilities/RPointer.R")


MakeGramTreeDriver <- function( grams )
{

    kGrams = lapply(names(grams), function(grams) strsplit(grams, ' ')[[1]])
    firstGramsFactor = factor(vapply(kGrams, `[[`, '', 1))
    return( MakeGramTree(grams, kGrams) )
}

MakeGramTree <- function( kfreqs, kGrams )
{
    if(length(kGrams) == 1 && is.na(kGrams[[1]][1]))
        return(list(freq = kfreqs[[1]], children = NULL))

    firstGramsFactor = factor(vapply(kGrams, `[[`, '', 1))
    kGramchildren = Map(function(freqs, grams) {
        grams = lapply(grams, function(x) x[2:length(x)] )
        MakeGramTree(freqs, grams)
    },split(kfreqs, firstGramsFactor), split(kGrams, firstGramsFactor))


    return( list(freq = sum(kfreqs), children = kGramchildren) )
}

# kgramsSplitByFirstGram = split(lapply(splitGrams, function(x) {
#     x[2:length(x)]
# }), factor(vapply(splitGrams, `[[`, '', 1)))

# kGrams = lapply(names(grams), function(gram) strsplit(gram, ' ')[[1]])

# firstGramsFactor = factor(vapply(kGrams, `[[`, '', 1))
# kGramsTree = Map(function(grams, freqs) {
#     list(freq = sum(freqs),
#          leaves = Map(function(gram, freq)
#              {
#              list(freq = freq, tree = gram[2:length(gram)])
#          }, grams, freqs))
# }, split(kGrams, firstGramsFactor), split(grams, firstGramsFactor))
#kGrams = lapply(names(grams), function(gram) strsplit(gram, ' ')[[1]])
# testGrams = c(rep("of the",1), rep("of the hair",2), rep("of a",3), rep("of a dog",4), rep("at a butt",3))
# gramTest = tapply(rep.int(1,length(testGrams)), testGrams, sum)
#
# kGrams = lapply(names(grams), function(grams) strsplit(grams, ' ')[[1]])
# firstGramsFactor = factor(vapply(kGrams, `[[`, '', 1))
# kGramTreeFunc = MakeGramTree(grams, kGrams)





