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

sampleRun1 <- function (){

  oMatrix = rbind(c(1, 1978), c(25, 87))
  m = makeCacheMatrix(oMatrix)
  
  message(m$get())
  
  message("Attemp 1: ")
    output <- cacheSolve(m)
  
  message("Attemp 2: ")
    output <- cacheSolve(m) 
    output
  
}

sampleRun2 <- function (){
  
  oMatrix = rbind(c(1/3, -1/4), c(-1/5, 1/8))
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
#	125197887
#	Attemp 1: 
#	No cache found: running solve() function 
#	Attemp 2: 
#	Cache found: getting cached data
#	              [,1]          [,2]
#	[1,] -0.0017624537  4.007050e-02
#	[2,]  0.0005064522 -2.025809e-05
#	> sampleRun2()
#	Gets the value of the matrix: 
#	0.333333333333333-0.2-0.250.125
#	Attemp 1: 
#	No cache found: running solve() function 
#	Attemp 2: 
#	Cache found: getting cached data
#	     [,1] [,2]
#	[1,]  -15  -30
#	[2,]  -24  -40
