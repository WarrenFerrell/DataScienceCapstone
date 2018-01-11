CleanLine <- function(s, inEncoding = "UTF-8", outEncoding = "ASCII")
{
    s <- iconv(s, inEncoding, outEncoding, sub="") # remove non ASCII characters
    s <- tolower(s)
    s <- gsub(" ?([-'a-z]+) ?" , " \\1 ", s)  # place spaces before and after words
    s <- gsub("( [ ]+)|(\\x1A)" , " ", s)  #remove whitespace and SUB characters
}
CleanLine <- compiler::cmpfun( CleanLine )