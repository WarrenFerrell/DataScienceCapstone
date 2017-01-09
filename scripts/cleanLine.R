cleanLine <- compiler::cmpfun( function(s){
    s <- gsub("DEL", "", iconv(s, "UTF-8", "ASCII", sub="DEL")) # remove non ASCII characters
    s <- tolower(s)
    gsub("([-'a-z]+)" , " \\1 ", s)  # place spaces before and after words
} )
