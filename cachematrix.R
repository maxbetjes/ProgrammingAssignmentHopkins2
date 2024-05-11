## makeCacheMatrix initaites a matrix but also creates the baility to store its inverse in the cache. 
## This inverse can be recalled (or calculated if not already there) using cacheSolve().

## Initiates a matrix with the ability to save it its inverse (inv) in the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Gets the inverse of a CacheMatrix either from the cache of by solve() if that is not possible

cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}
