## Put comments here that give an overall description of what your
## functions do
##See line-by-line comments

## Write a short comment describing this function.
## This function creates an empty matrix then provides an environment to store the calculated inverse of a matrix. 

makeCacheMatrix <- function(x= matrix()) {
	inverse <- NULL									##initially assigning a NULL value to the object "inverse".  "inverse" will be re-set every time that makeCacheMatrix is called.
	set_matrix <- function(y){
		x<<- y										##the argument y is saved as x in the higher environment
		inverse <<- NULL
	}
	get_matrix <- function() x 						##get_matrix is a function that returns the value of x
	set_inverse <- function(solvedinverse) inverse <<- solvedinverse 	##the argument solve which will be calculated to be the value of the inverse is saved as "inverse" in the higher environment.  This will be called in cacheSolve function below. 
	get_inverse <- function() inverse					##returns the inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix, 
		set_inverse = set_inverse, 
		get_inverse = get_inverse) 
	}
	


## Write a short comment describing this function
##This function checks to see if the inverse has already been calculated and returns the cached inverse if it has. Otherwise it calculates the inverse. 
cacheSolve <- function(x,...) {						##the function takes x as the argument which is defined as the matrix in the first makeCacheMatrix function
	inverse <- x$get_inverse()						##this performs the get_inverse function which just returns the inverse from that was set to the higher environment in the set_inverse function...why do you need the x in front of getinverse? 
	if(!is.null(inverse)){							##if inverse is NOT NULL (i.e. already calculated)...
		message("getting cached data") 				##kind of confusing that we display a message "Getting cached data" for the operation that's the quicker of the two (getting the inverse from the cache compared with actually calculating the inverse). We should have the message "Calculating inverse" for when we have to do that. Just sayin...
		return(inverse) 							##returns the saved value for inverse from the higher environment (cache) if it's NOT NULL. Note that the return() function ends the cacheSolve function and the remaining lines of code are not exercised. 
	}
	data <- x$get_matrix()							##we only reach this code if x$get_inverse returns NULL above.  get_matrix just returns the matrix x. 
	inverse <- solve(data,...)						##solve() will compute the inverse. Why can't you just do: inverse <- solve(x,...)??? ....why do we have to create "data" that stores x???
	x$set_inverse(inverse) 							##this passes the calculated inverse as an argument through the set_inverse function that we defined (it will replace the argument "solvedinverse" and store it as "inverse")
	inverse
}
##when you're done, fork the class respository on Github and then clone it to your desktop. It will ask where you want to save the file...the default is under thispc>documents>Github.  Then just go to that folder, open up the text file named cachemean and add in your code. Next open up Gitbash and run code to upload...
##question: in the test forum, when you call get_mean (in this case, get_inverse) does it have to go through the entire MakeCacheMatrix????? Set_inverse saved it in the higher environment so it would at least have to go up there for the inverse, right??? 
##cd to get to the home directory
##ls to see the files in the home directory
##cd documents/GitHub/ProgrammingAssignment2
##ls to see the files in your directory
##git clone <<URL of the REPO (which is on the right side of the page of the online repo)>>.  Note: paste using "insert"
##git status to show you the files that you're tracking and which ones are ready for committing 
##git commit - m "comment on your changes here"
##git status to make sure it took. 
##git log
##git push
