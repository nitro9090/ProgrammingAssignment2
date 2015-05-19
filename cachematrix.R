# These functions allow a user to cache a matrix and then find the inverse of
# said matrix using the 2 included functions.  To use, first upload the matrix
# into the makeCacheMatrix function and then run cacheSolve to solve for and/or
# call the inverse of the matrix.  Additionally, if cacheSolve has been run once
# already, the inverse can be recalled using the getInv.

# The makeCacheMatrix function caches a matrix and then allows the user to get
# and set the matrix, as well as get and set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
   matInv <- NULL
   
   setMat <- function(newMatrix = matrix()) {
      x <<- newMatrix
      matInv <<- NULL
   }
   getMat <- function() {
      x
   }
   setInv <- function(invIn = matrix()){
      matInv <<- invIn
   }
   getInv <- function() {
      matInv
   }
   list(setMat = setMat, getMat = getMat, setInv = setInv, getInv = getInv)
}


# The cacheSolve function either takes in a matrix or a makeCacheMatrix object.  For matrices,
# it solves the inverse.  For makeCacheMatrix objects, it checks if the inverse is already uploaded.
# If it is it outputs the inverse, otherwise it calculates it and uploads it to makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   if(is.matrix(x)){
      inv<-solve(x,...)
   }
   else {
      inv <- x$getInv()
      if(!is.null(inv)){
         message("The inverse was cached")
      }
      else {
         mat <- x$getMat()
         inv <- solve(mat,...)
         x$setInv(inv)
         message("The inverse was not cached")
      }
   }
   inv
}
