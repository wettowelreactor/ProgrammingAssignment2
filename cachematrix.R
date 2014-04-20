## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly


## The first function, makeCacheMatrix creates a special "martix", which is
## really a list containing a function to:
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # initalize the inverse to NULL
    inverse <- NULL
    
    # Set data function for creating a new Matrix
    set_data <- function(matrix) {
        # Set the internal data variable using the superassignemnt operator
        data <<- matrix
        # Clear out any cached inverses as this is new data
        inverse <<- NULL
    }
    
    # Get data function, returns the stored matrix
    get_data <- function() {
        data
    }
    
    # Set inverse function, sets the calculated inverse of the function.
    # This assumes that the data stored is the proper inverse for the stored 
    # matrix.
    set_inverse <- function(inv) {
        inverse <<- inv
    }
    
    # Get the cached inverse
    get_inverse <- function() {
        inverse
    }
    
    # Sets up the the functions above as callable methods of this object
    list(set_data = set_data, 
         get_data = get_data,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips
## the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the set_inverse function. Assumes
## that the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
