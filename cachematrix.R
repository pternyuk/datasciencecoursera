## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    OrigMatrix <<- y
    InvMatrix <<- NULL
  }
  get <- function() OrigMatrix
  setInv <- function(mean) InvMatrix <<- mean
  getInv <- function() InvMatrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$getInv()
  
  if(!is.null(InvMatrix)) {
    message("It's cached data!!!")
    return(InvMatrix)
  }
  data <- x$get()
  InvMatrix <- solve(data)
  x$setInv(InvMatrix)
  InvMatrix
}
