
makeCacheMatrix <- function(x = matrix()) {
  b <- FALSE               # status of cache is True if Inverse matrix has been cached 
  i <- matrix()            # i : Inverse Matrix
  
  set <- function(y) {     # set stores matrix and boolean of Inverse matrix at the
    x <<- y                # scope of the main function 
    b <<- FALSE
  }
  get <- function() x      # get returns the matrix of the main function
  
  setinv <- function(inverse)  # setinv stores the inverse matrix and its boolean
    { i <<- inverse            # at the scope of main function
      b <<- TRUE }
  
  getinv <- function() {      # getinv returns the matrix of inverse matrix and its boolean      
    list(b,i) }               # as a list
  
  list(set = set, get = get,  # list of 4 returned by the main function
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x) {
  cache <- x$getinv()   # reading of the object created with makeCacheMatrix 
  if(cache[[1]]) {      # check of the cache status (boolean), cached=True
    message("getting cached data")
    return(cache[[2]])  # output of inverse matrix 
                        # cache[[2]] is the inverse matrix cached
  }
  mat <- x$get()        # reading of matrix stored in object created with makeCacheMatrix
  i <- solve(mat)       # computation of inverse matrix 
  x$setinv(i)           # result is cached
  i                     # output of inverse matrix 
}
