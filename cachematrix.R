## Put comments here that give an overall description of what your
## functions do

## get a matrix as a input
## have two variables for saving the matrix and its inverse
## provide four functions for setting / getting them
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL # inverse of matrix x
    
    # function for setting x
    set <- function(new_x) { 
        x <<- new_x
        inv_x <<- NULL
    }
    
    # function for getting x
    get <- function() x
    
    # function for setting 'inv_x'
    setInv <- function(new_inv) inv_x <<- new_inv
    
    # function for getting 'inv_x'
    getInv <- function() inv_x
    
    # returns a list of functions
    list(set = set, 
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## get a function as a input
## returns the inverse of the matrix saved in the function
## if the inverse is already computed, returns the cached data
cacheSolve <- function(func, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if inverse matrix is cached
    inv <- func$getInv() 
    
    # if there is cached inverse matrix,
    if(!is.null(inv)) { 
        message("getting cached data")
        return(inv) # return the cached inverse matrix
    }
    
    # if there is no cached inverse matrix,
    mat <- func$get() # get the matrix value
    inv <- solve(mat) # compute the inverse matrix
    func$setInv(inv) # cache the computed inverse matrix
    inv # return the computed inverse matrix
}
