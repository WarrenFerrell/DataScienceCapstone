#Rprof('lotsOfVariables.out')
lotsaVariables <- function(x, size){
    y = c(x, rep('y', size))
    z = c(y, rep('z', size))
    a = c(z, rep('a', size))
    b = c(a, rep('b', size))
    c = c(b, rep('c', size))
    c
}
oneVariable <- function(x, size){
    x = c(x, rep('y', size))
    x = c(x, rep('z', size))
    x = c(x, rep('a', size))
    x = c(x, rep('b', size))
    x = c(x, rep('c', size))
    x
}
size = 1000
system.time({ replicate(n = 10000, lotsaVariables('x', size))})
system.time({ replicate(n = 10000, oneVariable('x', size))})
#Rprof(NULL)