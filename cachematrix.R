## pair of function that will cache the inverse of matrix
## functions will create a special objected "matrix" that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv<- NULL
        set <- function(y)
        {
x<<-y
                inv<<-NULL
                }
        get <- function() x
                setInverse<-function(solveMatrix) inv <<-solveMatrix
                        getInverse<-function() inv
                                list(set = set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## The function will compute the inverse of special "matrix" which is returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv))
                {
                message("getting the cached data")
                return(inv)
                }
        data<-x$get()
        inv<-solve(data)
        x$setInverse(inv)
        inv
                
}
