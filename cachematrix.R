#The makeCacheMatrix function creates a special "matrix" object that can cache its inverse
#The function stores a matrix X in the memory

makeCacheMatrix = function(X = matrix()) {
  Inv = NULL
  Set = function(y) { #sets data
    X <<- y
    Inv <<- NULL
  }
  Get = function() X #gets data
  SetInverse = function(Inverse) Inv <<- Inverse #sets the inverse of the data
  GetInverse = function() Inv #gets the inverse of the data
  list(Set = Set, Get = Get,
       SetInverse = SetInverse, 
       GetInverse = GetInverse)
} #returns list of functions

#cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
#The function first checks to see if the inverse has already been calculated.
#If matrix has not been changed and the inverse has already been calculated, then the cacheSolve
#should retrieve the inverse from the cache. Otherwise, it calculates the inverse of the data
#and sets the inverse in the cahce via the SetInverse function

cacheSolve = function(X, ...) {
  Inv = X$GetInverse()
  if(!is.null(Inv)) { #checks to see if inverse has already been calculated
    message("getting cached data")
    return (Inv)
  }
  data = X$Get() #Since the inverse has not been calculated, must first get the data
  Inv = solve(data, ...)
  X$SetInverse(Inv)
  Inv
}
