## makeCacheMatrix creats an object that can cache the inverse of the matrix supplied to it

##cacheSolve returns the inverse of the matrix object
## First it checks if the inverse has already been computed
## if it is then return the inverse from cache otherwise compute it and store the inverse in cache


## creates an "special matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns inverse of "special matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)){
        print("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
