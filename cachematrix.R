## Create two functions that provide the framework to cache the values
## that make up a matrix then calculate it's inverse, then store the 
## inverse in cache to facilitate non redundant calculations.
## 

## to use these functions first assign a matrix to a variable:
## mx<-matrix(c(1,2,3,0,1,4,5,6,0), nrow=3, ncol=3)
## Then call the first function:
## mymx<-makeCacheMatrix(mx)
## To verify it worked, and view the matrix:
## mymx$get()
## Then to cache an solve the solution run:
## cacheSolve(mymx)

makeCacheMatrix <- function(x = matrix()) {
## Function that creates four sub functions to: set, get, setsoln, 
## and getsoln.  get and set work with the matrix provided and 
## setsoln and getsoln worh with the calculated solution to the 
## matrix's inverse.
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsoln <- function(solve) m <<- solve
    getsoln <- function() m
    list(set = set, get = get,
         setsoln = setsoln,
         getsoln = getsoln)
}


cacheSolve <- function(x, ...) {
## Function to verify if a solution has been calculated and to either
## get the solution or store then get the solution from cache.
  
    m <- x$getsoln()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsoln(m)
    m
}
