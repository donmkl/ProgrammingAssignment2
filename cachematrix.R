
#Below are two functions that are used to create a special object
        #that stores a numeric matrix and cache's its inverse.

# makecacheVector creates a special "matrix", 
        #which is really a list containing a function to:
                #set the value of the matrix
                #get the value of the matrix
                #set the value of the inverse
                #get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # mi is the matrix inverse reset it to NULL
        mi <- NULL
        #define sub functions
        #save y as x
        #reset mi (matrix inverse)
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        #get the matrix
        get <- function() x
        #set matrix inverse from local variable mean if it exists
        setinverse <- function(inverse) mi <<- inverse
        #get the mean from mi
        getinverse <- function() mi
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The following function calculates the inverse of the 
        #special "matrix" created with the above function. 
        #However, it first checks to see 
                #if the inverse has already been calculated. 
                #If so, it gets the inverse from the cache 
                #and skips the computation. Otherwise, 
                #it calculates the inverse of the data 
                #and sets the value of the inverse in the 
                #cache via the setmean function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mi = x$getinv()
        
        #use existing matrix inverse if exists
        if(!is.null(mi)) {
                message("getting cached data")
                return(mi)
        }
        #otherwise calculate the matrix inverse
        data <- x$get()
        mi <- solve(data, ...)
        x$setinverse(mi)
        mi
}
