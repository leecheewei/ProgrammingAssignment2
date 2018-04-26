##makeCacheMatrix builds a set of functions and returns the functions within a list to the parent environment

makeCacheMatrix<-function(x=matrix()) 
{	
	i<-NULL 				## initialize i (inverse) and x (original)

	##Next few lines define the getters and setters functions for objects of type makeCacheMatrix()

	set<-function(y=matrix())
	{
		x<<-y
		i<<-NULL
	}
		
	get<-function() x
	setinverse<-function(inverse) i<<-inverse
	getinverse<-function() i

	
	##Return the list of functions to the parent environment. This technique allows use of $ operator to access each function in the list.

	return(list(set=set,				## gives the name 'set' to the set() function defined above
			get=get,				## gives the name 'get' to the get() function defined above
			setinverse=setinverse,		## gives the name 'setinverse' to the setinverse() function defined above
			getinverse=getinverse))		## gives the name 'getinverse' to the getinverse() function defined above
}


##cacheSolve solves the inverse matrix from an object of type makeCacheMatrix()

cacheSolve<-function(x,...)
{
	i<-x$getinverse()					## attempt to obtain the inverse matrix from an object of type makeCacheMatrix(). makeCacheMatrix()sets the cached inverse to NULL whenever a new matrix is set into the object
	if(!is.null(i))					## if we have a valid and cached inverse, we can return it to the parent environment
	{
		message('getting cached data')
		return(i)
	}
	data<-x$get()					## if i is NULL, cacheSolve() gets the matrix from the input object, solves the inverse, and uses the setinverse()to set the inverse in the input object
	i<-solve(data,...)    ## get the inverse matrix of x
	x$setinverse(i)
	i							## return the inverse matrix
}