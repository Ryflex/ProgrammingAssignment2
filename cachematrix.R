## This set of functions cache the inverse of a  matrix

## makeCacheMatrix creates a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize property for inversion
        m <- NULL
        
        ## Set the matix
        set <- function(y) {
                x <<- matrix
                m <<- NULL
        }
        
        ## Get the matrix
        get <- function() x

        ## Set the inverse of the matrix
        setInverse <- function(inverse) m <<- inverse
        
        ## Get the inverse of the matrix
        getInverse <- function() m
        
        ## List of functions methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve checks if the matrix is already cached. If so it returns the
## cached version. If not it calculates the inverse of the matrix and caches it
cacheSolve <- function(x, ...) {
        
        ## Return matrix inversed of x variable
        m <- x$getInverse()
        
        ## If cached matrix is available return it
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## Else create the inverse and cache it, return inversed matrix
        else {
                data <- x$get()
                
                m <- solve(data) %*% data
                                
                x$setInverse(m)
                
                m
        } 
        
}
