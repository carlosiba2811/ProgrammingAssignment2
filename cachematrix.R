## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        
        i <-  NULL 
        
        ## Set the matrix
        set <-  function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## Get the matrix
        get <- function(){
                 m
        }
        
        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
     
        ## Get the inverse of the matrix
        getInverse <- function() {
                i
        }

        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
         
        if( !is.null(m) ) {
            message("getting cached data")
            return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()

        ## Calculte the inverse
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setInverse(m)
        
        ## Return the matrix
        m
}
