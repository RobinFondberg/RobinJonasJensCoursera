#R Programming, Coursera
#Assignment 2, week 3
#Robin Fondberg

makeCacheMatrix <- function(x) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setInverse<-function(inv) m<<- inv
        getInverse<-function() m
        list(set=set, get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
        m<-x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data, ...)
        x$setInverse(m)
        m
}
