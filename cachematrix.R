
#### The makeCacheMatrix() function will create an object composed of four functions (set, get, setsolve, getsolve) and one matrix 
#### created by argument "x" and according to the number of rows and columns supplied at arguments "nrow" and "ncol" respectively

makeCacheMatrix <- function(x = matrix()) {
  ### Matrix
  ## The user should input a matrix as the argument for makeCacheMatrix()
  matrix <- x
  print("Matrix to be used:")
  print(matrix)
  # The "s" variable refers to the inverse of the matrix that will later be obtained by cacheSolve. Here, we make sure to reset it
  # so everytime we change the matrix it will not interact with a previous solution 
  s <- NULL
  ### set()
  # The set() function will only be used if the user decides to change the matrix after running makeCacheMatrix() with a specific x
  set <- function(y) {
    matrix <<- y
    s <<- NULL
  }
  ### get()
  # This function will print the matrix currently being used 
  get <- function() matrix
  ### setsolve()
  # This function will work in tandem with cacheSolve() to solve the matrix in this object
  setsolve <- function(solve) s <<- solve
  ### getsolve()
  # This function will work in tandem with cacheSolve() to provide the inverse of the matrix in this object
  getsolve <- function() s
  if (is.null(s)) {
    message("No cached inverse for this matrix")
  }
  ### Message
  message(paste("Functions set(), get(), setsolve(), and getsolve() were loaded. To access these functions, save this as an object"))
  ### Output (list)
  ## This is the actual output of the entire function. This enables the user to call for the individual nested functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


#### The cacheSolve() function will use the matrix created by makeCacheMatrix() and generate its inverse. 
#### If this calculation was already made previously, the function will just load the "s" variable, which would 
#### contain the solution of the previously identical run

cacheSolve <- function(x, ...) {
  ## Access "s" to verify if it exists. If it does, this calculation was already made and the function returns the cached result
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Inverse already calculated. Loading cached data")
    print("The reverse for this matrix is:")
    return(s)
  }
  ## If "s" is NULL, get the matrix and apply the solve() function
  matrix <- x$get()
  s <- solve(matrix, ...)
  x$setsolve(s)
  ## Return a matrix that is the inverse of 'x'
  print("The reverse for this matrix is:")
  print(s)
  message("Caching the result")
}