## This function takes argument of type matrix and creates a special matrix
## which is really a list containing function to set the value of matrix 
## get the value of matrix set the inverse of matrix get the inverse of matrix  

makeCacheMatrix <- function(argMatrix = matrix()) {

        invOfMatrix <- NULL ## function variable invOfMatrix is initialized
        
        ## This function sets argMatrix to input value and invOfMatrix is initialized 
        set <- function(y) {
                argMatrix <<- y
                invOfMatrix <<- NULL
        }
        
        ## This function gets "argMatrix" from environment 
        ## as illustrated in scoping
        get <- function() argMatrix
        
        ## This function sets "invOfMatrix" (inverse of matrix) 
        ## i.e. the out of " R - solve"
        setInverse <- function(arg_inverse) {
                invOfMatrix <<- arg_inverse
        }

        ## return "invOfMatrix" value
        getInverse <- function() invOfMatrix
        
        ## List is created for function
        ## when setArgs is called set function is executed
        ## when getArgs is called get function is executed
        ## when setInverseOfMatrix is called setInverse function is executed
        ## when getInverseOfArgs is called getInverse function is executed
        list(set = set
             ,get = get
             ,setInverseOfMatrix = setInverse
             ,getInverseOfMatrix = getInverse) 
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has been already calculated (and the matrix has not been changed), then cacheSolve retrieves
## the inverse from cache. 

cacheSolve <- function(x, ...) {
        
        ## get cached value
        inv <- x$getInverseOfMatrix()
        
        ## if the value is not null return cached value
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## get matrix - input value to makeCacheMatrix. 
        mat <- x$get()
        
        ## compute algebric matrix inversion by calling  R - Solve
        inv <- solve(mat, ...)
        
        ## Cache the inverse matrix 
        x$setInverseOfMatrix(inv)
        
        ## function return value
        inv
}
        