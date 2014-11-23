## So, this is my Cache making function
# . . I hope. 
#
#Credit where credit is due: As you will no doubt see it is largely based on the Vector Mean examples, 
#but with different objects and processes.  I also got a lot of mileage out 
#of the example/tutorial Bill Hilton posted on the class forum -- thanks Bill! 
#That said, any mistakes made are mine and mine alone ;)
# 

#The makeCacheMatrix function will (hopefully) provide the back end necessary 
# to run the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL 							# create empty placeholder variable
        set <- function(y) {				# nested function
                x <<- y						# set variable internal to function (search in function first)	
                i <<- NULL					# ""
        }
        get <- function() {x}								# returns value of original matrix
        setinverse <- function(inverse) {i <<- inverse}		# called to access and store the inverse value
        getinverse <- function() {i}							# will return the cashed value when accessed later
        list(set = set, get = get,							# access list for when makeCacheMatrix is called
             setinverse = setinverse,						
             getinverse = getinverse)
}


#The cacheSolve function will (hopefully) Return a matrix that is the inverse of 'x' cache the value 
# and return it from the cache without re-calculating it if an idetical request is made. 

cacheSolve <- function(x, ...) {	# x is object made by makeCacheMatrix    
        i <- x$getinverse()			#  accesses the object 'x' and gets the value of the inverse
        if(!is.null(i)) {			# if inverse was already cached (not NULL)
                message("getting cached data")
                return(i)
        }
        data <- x$get()				# this is run only if x$getinverse() returned NULL
        i <- solve(data, ...)		# calcualtes the inverse (the first time)
        x$setinverse(i)				# stores inverse matrix in "i"
        i							# returns value of i, and as the last called thing should print i


}