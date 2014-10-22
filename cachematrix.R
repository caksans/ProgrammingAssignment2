
## makeCacheMatrix is a function that takes a matrix agrument
## and creates a special matrix object that can cache its own inverse.

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)

}

## The cacheSolve function computes the inverse of the special matrix
## returned by makeCacheMatrix, above.  If the inverse has already been
## calculated and the matrix has not changed, the the cacheSolve function
## should retrieve the inverse from the cache.

cacheSolve <- function(x = matrix (), ...) {
        ## Return a matrix that is the inverse of 'x'

	m<-x$getmatrix()
	if(!is.null(m)){
		print("Using Cached Data")
		return(m)
	}
	matrix<-x$get()
	m<-solve(matrix,...)
	x$setmatrix(m)
	return(m)
}
