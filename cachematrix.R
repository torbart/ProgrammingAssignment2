## The two below functions create a list of functions to store matrix data,
## assess if there is already an inverse calculation, and then either
## print the cached data (calculated inverse) or calculate with new
## data and print that result.

## This function creates a vector of class list that contains
## four functions that can be easily called on in later functions.

makeCacheMatrix <- function(x = matrix()) { ## sets x as an object
                                            ## class matrix by default
        ## sets inv as an object with no data so it can be used
        ## in the lower functions.
        inv <- NULL
        
        ## creates set matrix function to allow us to change the matrix
        ## with ease in other functions (i.e. cacheSolve) without
        ## having to rerun this entire function.
        smat<- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## creates get matrix function to allow us to easily access 
        ## the current matrix at a later time.
        gmat <- function() x
        
        ## creates set inverse function to calcualte the inverse of the
        ## input matrix and to allow us to easily change the 
        ## inverse of the matrix in other functions (i.e. cacheSolve).
        sminv <- function(inverse) {
                inv <<- solve(inverse)
        }
        
        ## creates get inverse function to allow us to easily access
        ## the current inverse at a later time.
        gminv <- function() {
                inv
        }
        
        ## creates a vector class list of functions so we can call
        ## them with $ extract operator at a later time.
        list(smat = smat, gmat = gmat, 
             sminv = sminv, gminv = gminv)
}


## This function allows us to input an object with the information
## from the above function, check if there is already data, 
## if there is it will print the cached data. If there is not,
## it will recalculate with the new data and print that result.

cacheSolve <- function(x, ...) { ## creates function with variable input
        
        ## assigns inv object to the current matrix inverse from 
        ## makeCacheMatrix. 
        inv <- x$gminv()
        
        ## if statement to check if there is data already cahced in inv
        ## object, prints if there is already data there, moves to next
        ## code if not.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ## retrieves the current matrix from makeCacheMatrix if no
        ## cached data is available for inverse
        matrix <- x$gmat()
        
        ## assigns inv object with the resolution to the inverse of
        ## the assigned matrix from directly above.
        inv <- solve(matrix, ...)
        
        ## assigns the resolution directly above to the sminv object
        ## in makeCacheMatrix and then prints the result.
        x$sminv(inv)
        inv
}
