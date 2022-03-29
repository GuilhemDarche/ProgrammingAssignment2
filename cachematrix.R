## Put comments here that give an overall description of what your
## functions do
## the function allow to put in cache the inverse of a matrix entered by the user

## Write a short comment describing this function
## 1st initialisation of set function --> set
## 2nd allow to get the function --> get
## 3rd create the inverse matrix with solve --> setcache
## 4th allow to get the inverse matrix --> getcache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- as.matrix(y)
    m <<- NULL
  }
  get <- function() x
  setcache <- function(solve) m <<- solve
  getcache <- function() m
  list(set = set, 
       get = get, 
       setcache = setcache,
       getcache = getcache)
}


## Write a short comment describing this function
## m take the matrix of getcache 
## if not NULL then put a message "Getting cached matrix"
## else data take the value of matrix with the get function
## x allow to cache the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getcache()
  if(!is.null(m)) {
    message("Getting cached matrix")
    return(solve(m))
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}

## creation of mat1 and mat2 to test the code
mat1.data <- c(1:4)
mat1 <- matrix(mat1.data,nrow=2,ncol=2,byrow=TRUE)
mat2.data <- c(5:8)
mat2 <- matrix(mat2.data,nrow=2,ncol=2,byrow=TRUE)
mat3 <- c(10:20)

aMatrix <- makeCacheMatrix(mat2)
aMatrix$get()               # retrieve the value of the matrix
aMatrix$getcache()           # retrieve the value of m, which should be NULL
aMatrix$setcache(mat2)
cacheSolve(aMatrix)   

aMatrix$getcache()        
