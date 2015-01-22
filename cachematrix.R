
## makeCacheMatrix function creates a special "matrix" object can can catch its inverse. This object is really a list containing a function to
## 1)Set the value of the matrix
## 2)Get the value of the matrix
## 3)set the value of the inverse of the matrix
## 4)Get the value of the invers of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<-y
                inv<-NULL
        }
        get<-function() x
        setinverse<- function(inverse) inv<-inverse
        getinverse<- function() inv
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" object returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve<- function(x, ...) {
        inv<-x$getinverse() 
        if(!is.null(inv)){
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        message("getting cached inverse matrix")
        inv
}


