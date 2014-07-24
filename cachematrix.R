# Create a "cachematrix" which is a simple object that stores a matrix along
# with its inverse, if it has been calculated.

# Create an object that stores a matrix as well as its inverse
makeCacheMatrix <- function(x = matrix()) {

    # Variable for storing the inverse of the matrix
    matrix_inverse <- NULL

    # Store a new matrix (and clear any previously cached inverse)
    set <- function(y) {

        # Store new matrix as x
        x <<- y

        # Delete any previous copy of a stored inverse, since it would have
        # been calculated for a previous matrix
        matrix_inverse <<- NULL
    }

    # Retrieve the matrix
    get <- function() x

    # Store the inverse of the matrix
    setinverse <- function (inverse) matrix_inverse <<- inverse 

    # Retrieve the inverse of the matrix
    getinverse <- function () matrix_inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse
        )
}


# Solve a matrix for its inverse, unless it has already been solved
cacheSolve <- function(x, ...) {

    # Retrieve stored inverse
    inverse <- x$getinverse()

    # Check if inverse has been calculated previous and return it if so
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }

    # Retrieve original matrix
    my_matrix <- x$get()

    # Calculate inverse of the matrix
    inverse   <- solve(my_matrix)

    # Cache the calculated inverse
    x$setinverse(inverse)

    # Return the inverse
    return(inverse)
}
