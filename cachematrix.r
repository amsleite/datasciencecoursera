
## makeCacheMatrix stores a matrix X in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse



## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {

inverso <- NULL
set <- function(y) {
                        x <<- y
                        inverso <<- NULL

                   }



get <- function() x
setInverse <- function(inverso) inverso <<- inverso
getInverse <- function() inverso
list(set = set,
     get = get,
     setInverse = setInverse,
     getInverse = getInverse)



}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inverso <- x$getInverse()
if (!is.null(inverso)) {
                        message("getting cached data")
                        return(inverso)
                        }
mat <- x$get()
inverso <- solve(mat, ...)
x$setInverse(inverso)
inverso


}



##testing
basic_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
basic_matrix$get()

basic_matrix$getInverse()

cacheSolve(basic_matrix)

basic_matrix$getInverse()

