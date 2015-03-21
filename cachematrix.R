## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) {
  
  # set the myInvertedMatrix to NULL, var will be used to store the the inverted Matrix    
  myInvertedMatrix <- NULL
  
  # 1. Create the set function: sets the value of the vector and reset the value of myInvertedMatrix to NULL 
  set <- function(y) {
    myMatrix <<- y
    myInvertedMatrix <<- NULL
  }
  
  # 2.  Create the get function: get the value of the matrix
  get <- function() myMatrix
  
  # 3.  Create the setInverse function: 
  setInverse <- function(solve) myInvertedMatrix <<- solve
  
  # 3.  Create the setInverse function: 
  getInverse <- function() myInvertedMatrix
  
  # use the list function, to make a list of functions, so you can call the defined functions for the vector, 
  # ie faking good ol' OO-Programming
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  myMatrix <- x$getInverse()
  if(!is.null(myMatrix)) 
    {
      message("Cache found: getting cached data")
      # return and exit function 
      return(myMatrix )
  }
  
  message("No cached data: running solve() function ")
  data <- x$get()
  myMatrix <- solve(data, ...)
  x$setInverse(myMatrix)
  myMatrix
}

## Sample run:
## > x = rbind(c(5, 6), c(8,9))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]    5    6
## [2,]    8    9

##  cacheSolve(m)
## No cached data: running solve() function 
## [,1]      [,2]
## [1,] -3.000000  2.000000
## [2,]  2.666667 -1.666667
## > cacheSolve(m)
## Cache found: getting cached data
## [,1]      [,2]
## [1,] -3.000000  2.000000
## [2,]  2.666667 -1.666667

