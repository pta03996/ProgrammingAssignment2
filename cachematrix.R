## -- Assignment 2 --
## OBjective: this assignment will take advantage of the scoping rules of the R
## What we gonna do: write a function that cache resulting in reducing computation time
## 1. makeCacheMatrix =>  make a cache fo a matrix that is already inversed
## 2. cacheSolve => computes the inverse matrix from makeCacheMatrix


## make a cache fo a matrix
makeCacheMatrix <- function(mtx = matrix()) {
  inversedMatrix <- NULL
  
  setMatrix <- function(matrix) {
    mtx <<- matrix
    inversedMatrix <<- NULL
  }
  
  getMatrix <- function() {
    mtx
  }
  
  setInversedMatrix <- function(inverse) {
    inversedMatrix <<- inverse
  }
  
  getInversedMatrix <- function() {
    inversedMatrix
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInversedMatrix = setInversedMatrix,
       getInversedMatrix = getInversedMatrix)
}


## cacheSolve
## get the matrix from makeCacheMatrix
## if the matrix is the same, then retrieve the inverse from the cache
## if not, calculate

cacheSolve <- function(x, ...) {
  
  inversedMatrix <- x$getInversedMatrix()
  
  if(!is.null(inversedMatrix)){
    message('getting caches data')
    return(inversedMatrix)
  }
  
  newMatrix <- x$getMatrix()
  inversedMatrix <- solve(newMatrix)
  x$setInversedMatrix(inversedMatrix)
  inversedMatrix
}

