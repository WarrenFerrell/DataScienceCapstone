twitterPath = "data/en_US/twitter/en_US.twitter.txt"
blogsPath = "data/en_US/blogs/en_US.blogs.txt"
newsPath = "data/en_US/news/en_US.news.txt"

longer <- function( currentLongest, newlen, dataset ){
    if( newlen > currentLongest[[1]])
        return( list(newlen, dataset) )
    else
        return( currentLongest )
}


nlines <- 0
#longest <- list(0, "")
nlove <- 0L
nhate <- 0L
nchess <- 0L
chessStr <- "A computer once beat me at chess, but it was no match for me at kickboxing"



#twitter = list()
numchar <- 0
twitterCon = file(twitterPath, "rt")
while(length(line <- readLines(twitterCon, n = 1L)) > 0) {
    nlines = nlines + 1
    numchar = numchar + nchar(line)
    #longest <- longer(longest, nchar(line), "twitter")
    # nlove <- nlove + grepl(pattern = "love", x = line)
    # nhate <- nhate + grepl(pattern = "hate", x = line)
    # if( grepl(pattern = "^.*biostats.*$", x = line) )
    #     biostatline <- line
    # nchess <- nchess + grepl(pattern = chessStr, x = line)
    # #twitter = c(twitter, line)
}
print(paste("twitter:",nlines))
print(numchar)
close(twitterCon)

nlines <- 0
blogsCon = file(blogsPath, "rt")
while(length(line <- readLines(blogsCon, n = 1L)) > 0) {
    nlines = nlines + 1
    numchar = numchar + nchar(line)
    #longest <- longer(longest, nchar(line), "blogs")
}
print(paste("blogs:",nlines))
print(numchar)
close(blogsCon)


nlines <- 0
newsCon = file(newsPath, "rt")
while(length(line <- readLines(newsCon, n = 1L)) > 0) {
    nlines = nlines + 1
    numchar = numchar + nchar(line)
    #longest <- longer(longest, nchar(line), "news")

}
print(paste("news",nlines))
print(numchar)
close(newsCon)
