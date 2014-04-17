## The object "makeCacheMatrix" is a function that assigns a list with four functions to a special matrix 'x'


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {              # set() is a function that creates placeholder for the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x               # get() is a function to call the matrix values
        setinverse <- function(inverse) m <<- inverse      # creates a placeholder for the inverse
        getinverse <- function() m                         # is NULL before the function "cacheinverse" is called
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve is a function that calculates the inverse of the matrix 'x' 
# and stores the value in the cache of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getinverse()               
        # cacheSolve first checks with getinverse() whether it already exists
        if(!is.null(m)) {                         
        # if it is not NULL it will use it, otherwise it calculates it with solve()
                message("getting cached data")
                return(m)
        }
        data <- x$get()                   
        # the "data" comes from the get() function that gets the values of the matrix
        m <- solve(data, ...)               
        # solve() is the function to calculate the inverse of the matrix
        x$setinverse(m)                     
        # the inverse of the matrix "m" is assigned to x with the setinverse() function
        m
}
