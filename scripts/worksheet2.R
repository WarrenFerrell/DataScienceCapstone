#gList <- env2list(gramModel)

a <- new.env()
a[['1']] <- 'test1'
a[['2']] <- new.env()
a[['2']][['1']] <- 'test2'
a[['2']][['2']] <- new.env()
a[['2']][['2']][['1']] <- 'test3'
