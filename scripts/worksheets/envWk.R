library(pryr)
#gList <- env2list(grm1mTree)
m1 <- new.env()
m1[['a']] <- new.env()
m1[['a']][['#']] <- 9
m1[['a']][['b']] <- new.env()
m1[['a']][['b']][['#']] <- 8

m2 <- new.env()
#m2 <- m1
m2[['a']] <- new.env()
m2[['a']][['#']] <- 1
m2[['a']][['b']] <- new.env()
m2[['a']][['b']][['#']] <- 2
m2[['c']] <- new.env()
m2[['c']][['#']] <- 10

# s1 <- new.env()
# print(object_size(s1))
# s1[['#']] <- 1
# print(object_size(s1))
# s1[['a']] <- new.env()
# print(object_size(s1))
# s1[['b']] <- new.env()
# print(object_size(s1))
