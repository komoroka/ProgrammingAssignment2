## write a pair of functions that caches the inverse of a matrix 

## reference : https://stackoverflow.com/questions/24904683/caching-the-mean-of-a-vector-in-r

## create a special matrix object that can cache its inverse 

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  
  # make sure this only takes a matrix parameter
  setinv <- function(i=matrix()) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## computes the inverse of the special matrix returned by makeCacheMatrix
## if the inverse has already been calculated then cacheSolve should retrive
## the inverse from the cache .

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


# Examples 
#
# # sample 2x2 matrix 
# m1 <- matrix(c(1,3,5,2),nrow=2)
# 
# #create the matrix object 
# mat1 <- makeCacheMatrix(m1)
# 
# # get the matrix
# mat1$get()
# 
# # the inverse should be null 
# mat1$getinv()
# 
# # cache the inverse
# cacheSolve(mat1)
