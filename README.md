# In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean. The first function, makeVector creates a special "vector", which is really a list containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the mean
#get the value of the mean

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

# The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
    m <- x$getmean()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

# Main Assignment on lexical scoping (My matrix function)

## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Explanation of the code above;

# Caching is a technique used to store computed values so that they can be quickly retrieved without recalculating when the same computations are needed again
# makeCacheMatrix <- function(x = matrix()) {: This line defines a function named makeCacheMatrix with a default argument x set to an empty matrix. The function is intended to create a cache for a matrix and its inverse.inv <- NULL: Initializes a variable inv to hold the inverse of the matrix. It is initially set to NULL.
# set <- function(y) { x <<- y inv <<- NULL }: Defines a function named set inside makeCacheMatrix. This function takes a matrix y as an argument and assigns it to the variable x. Additionally, it sets the inverse (inv) to NULL. The use of <<- is for assigning the value in the global environment.
# get <- function() x: Defines a function named get inside makeCacheMatrix. This function returns the current value of the matrix stored in the variable x.
# setInverse <- function(inverse) inv <<- inverse: Defines a function named setInverse inside makeCacheMatrix. This function takes an inverse matrix as an argument and assigns it to the variable inv using <<-.
# getInverse <- function() inv: Defines a function named getInverse inside makeCacheMatrix. This function returns the current value of the inverse matrix stored in the variable inv.
# list(set = set, get = get, setInverse = setInverse, getInverse = getInverse): Creates and returns a list containing the four functions defined within makeCacheMatrix. This list acts as a cache, allowing users to manipulate and retrieve the matrix and its inverse using the provided functions (set, get, setInverse, getInverse).

# second function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

#  the cacheSolve function utilizes caching to check if the inverse of the matrix is already computed and stored in the cache. If so, it retrieves the cached value; otherwise, it computes the inverse, updates the cache, and returns the result. This approach can improve efficiency by avoiding redundant calculations when the inverse has already been computed and cached.

# cacheSolve <- function(x, ...) {: Defines the cacheSolve function that takes a cached matrix object (x) as its argument. The ellipsis (...) allows additional parameters to be passed to the solve function.

# inv <- x$getInverse(): Retrieves the cached inverse matrix from the input cached matrix object (x). If the inverse is already cached, it is stored in the variable inv.

# if (!is.null(inv)) { message("getting cached data") return(inv) }: Checks if the inverse matrix (inv) is not NULL. If it's not NULL, a message is printed indicating that cached data is being used, and the cached inverse is returned, bypassing the need for recalculation.

# mat <- x$get(): Retrieves the matrix (mat) from the cached matrix object (x). This matrix is used if the inverse is not already cached.

# inv <- solve(mat, ...): Computes the inverse of the matrix (mat) using the solve function. The additional parameters passed through the ellipsis allow customization of the inversion process.

# x$setInverse(inv): Updates the cache by storing the newly computed inverse (inv) in the cached matrix object (x). inv: Returns the computed inverse matrix.

# Testing the functions:

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()

my_matrix$getInverse()
cacheSolve(my_matrix)

my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

my_matrix$get()
my_matrix$getInverse()

cacheSolve(my_matrix)
my_matrix$getInverse()
