#Functions to create a matrix and cace its invrse

makeCacheMatrix <- function(x = matrix()) { #create function
        inverse1 <- NULL                             #assign null value to inverse
        set <- function(y) {
                x <<- y                 #globally assign inputted value to x, which would be taken into inverse calculation
                inverse1 <<- NULL            #globally set inv to NULL
        }
        get <- function() x #return value of matrix
        setinv = function(inverse2) inverse2 <<- inverse1 #set value
        getinv = function() inverse1 #output inverse value
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x) {
        
        inverse1 = x$getinv() #get any existing value of inverse from cache
        
        if (!is.null(inverse1)){ #check if existing value is there, if yes, return that value
                message("getting cached data")
                return(inverse1)
        }
        
       matrix.data = x$get() #if no existing value of inverse in cache get the matrix data
        inverse1 = solve(matrix.data) #calculate inverse
        
        x$setinv(inverse1) #set value in cache
        
        return(inverse1) #return the inverse
}
