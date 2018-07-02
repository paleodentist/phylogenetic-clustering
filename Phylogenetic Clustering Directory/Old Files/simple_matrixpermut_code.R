### Simple code for randomly & iteratively seeding matrices with 1s and 0s, for permutation tests
### Set some things up first

permut <- 10 # change if you need to do more/less permutations
genera <- length(ExtPuercan) # change to represent number of genera you need in the matrix for each time slice
extinctions <- 5 # change to represent # of genera going extinct in that timeslice

alpha_vector<-c()

for (i in 1:permut){
	
	mat<-matrix(0, nrow=genera, ncol=genera) # creates an empty matrix with dim genera
	rand_sample<-sample(length(mat), extinctions, replace=FALSE)
	mat[rand_sample]<-1
	# print(mat) # useful if you want to check it's doing the right thing
	
	# now you just need to generate the other matrix, and then calculate an alpha value (call is 'alpha_res' for now)
	# alpha_res<- # work this out later
	
	# load it into the empty vector
	alpha_vector<-c(alpha_vector, alpha_res)
	
}


