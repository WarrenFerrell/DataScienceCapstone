library(pryr)
source("scripts/RPointer.R")
MemTest <- function(i=1,n=3)
{
    if (i > n)
        return()
    print(paste0("i:",i, " mem_used:", mem_used(), " object_size(testObjPtr):",object_size(testObjPtr)))
    testObjPtr$value[1] <- 1
    MemTest(i+1,n)
}
MemTestPassObj <- function(i=1,n=3,testObjPtr)
{
    if (i > n)
        return()
    print(paste0("i:",i, " mem_used:", mem_used(), " object_size(testObjPtr):",object_size(testObjPtr)))
    testObjPtr$value[1] <- 1
    MemTestPassObj(i+1,n,testObjPtr)
}
MemTestLoop <- function(n=3)
{
    for(i in 1:n)
        print(paste0("i:",i, " mem_used:", mem_used(), " object_size(testObjPtr):",object_size(testObjPtr)))
    testObjPtr$value[1] <- 1
}
testObj <- 1:10000000
testObjPtr <- newPointer(testObj)
testObjPtr$value[1] <- 1
MemTest()
print("PassObj")
MemTestPassObj(testObj = testObjPtr)
print("Loop")
MemTestLoop()