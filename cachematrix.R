## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL #x is the matrix, m is the inverse one (for now it has a value of NULL)
    }
    get <- function() x
    setinv <- function(solve) m <<- solve #Store the inverse after calculation
    getinv <- function() m
    list(set = set, get = get,#Lexical scoping
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m) #Checking cache,; It won't make another calculation if the inverse already exist
    }
    data <- x$get()
    m <- solve(data, ...) #compute and cache
    x$setinv(m) 
    m        ## Return a matrix that is the inverse of 'x'
}
