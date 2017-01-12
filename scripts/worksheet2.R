#gList <- env2list(grm1mModel)

m1 <- new.env()
m1[['a']] <- new.env()
m1[['a']][['gfreq']] <- 9
m1[['a']][['b']] <- new.env()
m1[['a']][['b']][['gfreq']] <- 8

m2 <- new.env()
#m2 <- m1
m2[['a']] <- new.env()
m2[['a']][['gfreq']] <- 1
m2[['a']][['b']] <- new.env()
m2[['a']][['b']][['gfreq']] <- 2
m2[['c']] <- new.env()
m2[['c']][['gfreq']] <- 10