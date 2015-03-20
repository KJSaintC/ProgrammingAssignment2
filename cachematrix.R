## These functions cache and retrieve the value of the inverse of a designated
## matrix, x. Cacheing the matrix inversion can save time when referencing it
## in other functions.

## makeCacheMatrix() creates the objects that will cache the inverse and store
## them in the function's parent environment.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setsolve<-function(solve) m<<-solve
        getsolve<-function() m
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve,)
}


## cacheSolve() checks to see whether the value for the inverse for matrix x
## has already been cached. If so, it retrieves the cached version.If not,
## cacheSolve() calculates the inverse.

cacheSolve <- function(x, ...) {
       m<-x$getsolve()
       if(!is.null(m)){
               message("getting cached data")
               return(m)
       }
       data<-x$get()
       m<-solve(data,...)
       x$setsolve(m)
       m
}
