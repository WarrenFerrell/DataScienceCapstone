MemTest <- function(i=1,n=3)
{
    if (i > n)
        return()
    print(paste("i =",i, "mem_used", pryr::mem_used(),"pryr::object_size(testObj)",pryr::object_size(testObj)))
    testObj[1] <- 1
    MemTest(i+1,n)
}
MemTestPassObj <- function(i=1,n=3,testObj)
{
    if (i > n)
        return()
    print(paste("i =",i, "mem_used", pryr::mem_used(),"pryr::object_size(testObj)",pryr::object_size(testObj)))
    testObj[1] <- 1
    MemTestPassObj(i+1,n,testObj)
}
MemTestLoop <- function(n=3)
{
    for(i in 1:n)
        print(paste("i =",i, "mem_used", pryr::mem_used(),"pryr::object_size(testObj)",pryr::object_size(testObj)))
    testObj[1] <- 1
}
testObj <- 1:10000000
MemTest()
print("PassObj")
MemTestPassObj(testObj = testObj)
print("Loop")
MemTestLoop()
print("Compile first")
MemtestCompile <- compiler::cmpfun(MemTest)
MemtestCompile()