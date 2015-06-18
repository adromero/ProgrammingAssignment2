## The following two functions use lexical scoping to create a matrix 
## object (list) and then use that in order to calculate the inverse of that matrix.
## If the inverse has already been calculated, then the second function will 
## return the inverse from the cache.

## The makeCacheMatrix function creates a matrix object, which is a list
## of functions that will:
	## [Lines 16 - 19 ] sets the value for the matrix object
	## [Line 20] gets the value for the matrix object
	## [Line 21] sets the value for the matrix inversion
	## [Line 22] gets the value for the matrix inversion
	## [Line 23] creates a list of the functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
		}
	get <- function () x
	setinv <- function(inversion) m <<- inversion
	getinv <- function() m	
	list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## The cacheSolve function calculates the inverse of the matrix created in the
## makeCacheMatrix function. 
## [Lines 35 - 39] It first checks to see if the inverse has been calculated
## and if so will return the cached value. 
## [Line 40 - 43] If not, it will use the solve function to return the inverse of 
## the gotten value of the matrix created in makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getinv()   #returns value of matrix inversion by subsetting the main function
    if(!is.null(m)) {
      message("cached data found, retrieving...")
      return(m)
    }
    data <- x$get() # creates data by subsetting from main function
    m <- solve(data)
    x$setinv(m) # runs subsetted setinv function
    m
}
