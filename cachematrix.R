## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) {
  
  # set the myInvertedMatrix to NULL, var will be used to store the the inverted Matrix    
  myInvertedMatrix <- NULL
  
  # 1. Create the set function: sets the value of the matrix and reset the value of myInvertedMatrix to NULL 
  set <- function(y) {
    message ("sets the value of the matrix and reset the value of myInvertedMatrix to NULL")
    myMatrix <<- y
    myInvertedMatrix <<- NULL
  }
  
  # 2.  Create the get function: get the value of the matrix
  get <- function() myMatrix
  {
    message ("Gets the value of the matrix: ")
  }
  
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
    return(myMatrix)
  }
  
  message("No cache found: running solve() function ")
  
  # get the stored matrix
  data <- x$get()
  
  # run solve() function 
  myMatrix <- solve(data, ...)
  
  # run setIverse function 
  x$setInverse(myMatrix)
  
  # return value 
  return (myMatrix)
}

# function for testing 
sampleRun1 <- function (myMatrix = matrix){
  myrows = c(1, 2)
  mycolumns = c(5, 6)
  oMatrix = rbind (myrows, mycolumns)
  m = makeCacheMatrix(oMatrix)
  
  message(m$get())
  
  message("Attemp 1: ")
    output <- cacheSolve(m)
  
  message("Attemp 2: ")
    output <- cacheSolve(m) 
    output
}

# function for testing 
sampleRun2 <- function (myMatrix = matrix){
  myrows = c(1, -1/4)
  mycolumns = c(-1/4, 1)
  oMatrix = rbind (myrows, mycolumns)
  m = makeCacheMatrix(oMatrix)
  message(m$get()) 
  
  message("Attemp 1: ")
  output <- cacheSolve(m)

  message("Attemp 2: ")
  output <- cacheSolve(m)
  output
}

# > source('/ProgrammingAssignment2/cachematrix.R')
#	> sampleRun1()
#	Gets the value of the matrix: 
#	1526
#	Attemp 1: 
#	No cache found: running solve() function 
#	Attemp 2: 
#	Cache found: getting cached data
#	     myrows mycolumns
#	[1,]  -1.50      0.50
#	[2,]   1.25     -0.25
#	> sampleRun2()
#	Gets the value of the matrix: 
#	1-0.25-0.251
#	Attemp 1: 
#	No cache found: running solve() function 
#	Attemp 2: 
#	Cache found: getting cached data
#	        myrows mycolumns
#	[1,] 1.0666667 0.2666667
#	[2,] 0.2666667 1.0666667

