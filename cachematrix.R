## makeCacheMatrix creates a special matrix that can store its inverse.
## cacheSolve takes the matrix created above and either retrieves or calculates
## the inverse matrix.

## makeCacheMatrix object contains 4 accessor functions (get, set, getInverse, 
## setInverse). These functions either get or set the matrix or its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(inputMatrix) {
    x<<-inputMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseMatrix <<-inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks whether the input matrix has an inverse and only 
## calculates it if it is not present. It retrieves the inverse if an inverse
## exists.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    print("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
