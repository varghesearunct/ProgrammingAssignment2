## This function will store the inverse of the matrix and can be retrieved
## by calling it, and there by reducing the computation in finding the inverse

## This function will cache the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
	x<<-y
	m<<-NULL
	}
	get<-function() x
	setinverse<-function(inverse) m<<-inverse
	getinverse<-function() m
	list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## This function will retrieve the matrix inverse if already calculated otherwise will find the inverse


cacheSolve <- function(x, ...) {
        s<-x$getinverse()
	if(!is.null(s))
		{
			return(s)
		}
	q<-x$get()
	s<-solve(q)
	x$setinverse(s)
	s
}
