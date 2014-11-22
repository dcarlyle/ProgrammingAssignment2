## coursera R programming - assignment 2 - week 3
## load with - source("cachematrix.R")
##
## Matrix inversion is usually a costly computation and their may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
##
## The following pair of functions cache the inverse of a matrix.
##
## Assuming that the matrix supplied is always invertible.
##
## https://class.coursera.org/rprog-009/human_grading/view/courses/972583/assessments/3/submissions
## 
## Original Source from rdPeng
## https://github.com/1louder/ProgrammingAssignment2/blob/master/cachematrix.R
##
## Example usage
## > t <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)  # create a 3 by 3 matrix
## > source("matrix_cache.R")               # load functions
## > matrixObject <- makeCacheMatrix(t)     # make a cached matrix object from a normal Matrix t
## > 
## > cacheSolve(matrixObject) ## call it FIRST time to see it is NOT cached
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
## > invT <- cacheSolve(matrixObject)  ## call it SECOND time to see it is cached
## getting cached matrix
##      [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
##
## TEST if TRUE that t * t^-1 = Identity matrix
## > t %*% invT
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1
## 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 
makeCacheMatrix <- function(x = matrix()) {

	## initialise variable 'm' to hold inverted matrix
	m <- NULL
	
	## assign x the value of y, initialise m
    set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	## getter method/function for returning value of x
    get <- function() {
		return(x)
	}
	
	## Set Cache - set 'm' on env lexical scope with value 'v' 
	setinverse <- function(v) {
		m <<- v
	}

	## Get Cache - get 'm' on env lexical scope	
	getinverse  <- function() {
		return(m)
	}
	
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		## set local scoped 'm' to env scoped 'm' (cached 'm')
        m <- x$getinverse()
		
		## check that 'm' has a value, other than NULL
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
		
		## 'm' has a value and it is not NULL, so lets invert it with function 'solve'
        m <- solve(data, ...)
		
		## now cache 'm' for later
        x$setinverse(m)
        
		return(m)
}