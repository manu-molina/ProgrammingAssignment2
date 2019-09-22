
#Assignment Coursera Week 3. (Manuel Molina)


## The function described below, creates a special matrix object
##that can cache its inverse. This function is a list containing a function to: 
  ## 1. Set the value of the matrix.
  ## 2. Get the value of the matrix.
  ## 3. Set the value of the inverse.
  ## 4. Get the value of the inverse.

makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(y) {
    a <<- y
    inv <<- NULL
  }
  get <- function() a
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function returns the inverse of the matrix gived by makeCacheMatrix. 
## If the function has been calculated, this function will retrieve the inverse from cache. 

cacheSolve <- function(a, ...) {
  inv <- a$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- a$get()
  inv <- solve(data, ...)
  a$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

## Testing

## To test the two functions described above, I have created a matrix called A. 

x <- c(1,-2,2,3)
A <- matrix(x, nrow=2, ncol=2)

## Then I applied both functions (makeCacheMatrix and cacheSolve)

A1 <- makeCacheMatrix(A)
cacheSolve(A1)

cacheSolve(A1)

## The second time I apply the "cacheSolve" function, the inverse is taken from cache. 
## So both functions written in the beggining are okay. 




