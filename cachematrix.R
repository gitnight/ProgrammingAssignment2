#Functions to create a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) { #create function
        inv <- NULL                             #assign null value to inverse
        set <- function(y) {
                x <<- y                 #globally assign inputted value to x, which would be taken into inverse calculation
                inv <<- NULL            #globally set inv to NULL
        }
        get <- function() x #return value of matrix
        setinverse <- function(inverse) inv <<- inverse #set value
        getinverse <- function() inv #output inverse value
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse() #get any existing value of inverse from cache
        
        if (!is.null(inv)){ #check if existing value is there, if yes, return that value
                message("getting cached data")
                return(inv)
        }
        
       data <- x$get() #if no existing value of inverse in cache get the matrix data
        inv <- solve(data, ...) #calculate inverse
        
        x$setinverse(inv) #set value in cache
        
        inv #return the inverse
}
