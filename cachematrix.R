## Programming Assingment 2
## Two functions 
##  makeCacheMatrix - creates a list with functions
##  cacheSolve - finds inverse of matrix checking previously cached matrix


## creates a list with functions
## set the value of matrix
## get the value of matrix
## set the value of inverse
## get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) i<<- solve
  getmatrix<-function() i
  list(set=set, get=get, setmatrix=setmatrix,getmatrix=getmatrix)
}


## Calculates mean of matrix
## first checks to see if mean has already been calculated
## if so skips computation
## else calculates the mean

cacheSolve <- function(x, ...) {
  i<-x$getmatrix()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix<-x$get()
  i<-solve(matrix, ...)
  x$setmatrix(i)
  i
}

## --------------------------Test run commands----------------
## > test<- rbind (c(1.0, -0.25), c(-0.25,1))
## > z <- makeCacheMatrix(test)
## > cacheSolve(z)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## -----------if you again run then it says getting cached data-------
## > cacheSolve(z)
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
