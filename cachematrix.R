## This file conrains 2 function to operate with matrix. 
## makeCacheMatrix - create object with 4 methods. Get/Set Value of
## Original Matrix and Get/Set Value of Inv Matrix
## cacheSolve - return Inv Matrix of Original Matrix in makeCacheMatrix object.
## Inv Matrix returns from cache (if it was precalculeted) or from function solve

## Create object to operate with Matrix

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  set <- function(y) {
    ## Save in memory Original Matrix
    OrigMatrix <<- y
    ## Destroy Invent Matrix if Orig Matrix change value
    InvMatrix <<- NULL
  }
  ## Return Original Matrix from memory
  get <- function() OrigMatrix
  ## Save in memory Inv Matrix
  setInv <- function(Inv) InvMatrix <<- Inv
  ## Return Inv Matrix from memory
  getInv <- function() InvMatrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Calculate (or read from cache) and return Inv Matrix to Matrix in object x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Read Inv Matrix from object which was created by makeCacheMatrix function
  InvMatrix <- x$getInv()
  ## Check if there is Inv Matrix in object
  if(!is.null(InvMatrix)) {
    message("It's cached data!!!")
    ## Return Inv Matrix from cache
    return(InvMatrix)
  }
  ## Read Original Matrix from object which was created by makeCacheMatrix function
  data <- x$get()
  ## Calculate Inv Matrix
  InvMatrix <- solve(data)
  ## Save Inv Matrix in object which was created by makeCacheMatrix function
  x$setInv(InvMatrix)
  ## Return Inv Matrix
  InvMatrix
}
