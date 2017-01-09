library(microbenchmark); library(fastmatch)

#
# # a <- list(1,2,3)
# # b <- list(2,3,4)
# # c <- list(3,4,5)
#
#
# default <- function( toCheck = includedTerms, toSearch = topTerms ) {
#     toSearch <- list2env( toSearch )
#     for( i in seq_along( toCheck ) ) {
#         toSearch[[toCheck[[i]]]] <- 0
#     }
#     cat(toSearch, '; ')
# }
#
# package <- function( toCheck = includedTerms, toSearch = topTerms ) {
#     toSearch <- list2env( toSearch )
#     for( i in seq_along( toCheck ) ) {
#         toSearch[ fmatch(toCheck[i], names(toSearch))] <- 0
#     }
#
#     cat(toSearch, '; ')
# }
#
# testCheck <- c('a', 'b', 'c', 'd')
# testSearch <- list(a = 1, aa = 2, b = 3, bb = 4, c = 5, cc = 6, d = 7, dd = 8, e = 9, ee = 10 )
# #testSearch <- rep(testSearch, 2000)
# print( microbenchmark(default(testCheck, testSearch), package(testCheck, testSearch), times = 1L) )

gList <- env2list(gramModel)