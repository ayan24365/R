MakeCacheMatrix<-function(AyanM=matrix())
{
Inv<-NULL
set<-function(y) {
AyanM<<-y
inv<<-NULL
}
get<-function()AyanM
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


cacheSolve <- function(x, ...) {
        
makeCacheMatrix()
        
        inv = x$getinv()
        
                if (!is.null(inv)){
                message("Obtaining cached data")
                return(inv)
        }
        
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
