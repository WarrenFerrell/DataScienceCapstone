ngram_tokenizer <- function(s, k = 12) {
        x <- strsplit(as.String(s), ' ')[[1]]
        x <- x[ x != '' ]
        tokens <- list()
        if( length(x) > 1 ) {
			for( n in seq(2, k) ) {
				newGrams <- ngrams( x , n )
				for( gram in newGrams) {
					if( (gram[1] != '#') && (gram[n] != '#') && (gram[1] != 'NA') && (gram[n] != 'NA') ) {
						tokens <- c(tokens, list(gram)) #only include grams whose first and last term are known
					}
				}
			}
		}
        return( tokens ) #unroll nested list
}

