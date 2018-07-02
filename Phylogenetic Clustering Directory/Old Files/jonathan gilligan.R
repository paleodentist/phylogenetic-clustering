
make_random_matrices <- function(base_mat, n_runs) {
N <- dim(base_mat)[1]
n_ones <- (sum(base_mat) - N) / 2

jg_x <- combn(1:N,2)

r_matrices <- list()

for(i in 1:n_runs) {
  r_ind <- jg_x[,sample(ncol(jg_x), n_ones)]
  rmat <- matrix(0,ncol = N, nrow = N)
  for(j in 1:ncol(r_ind)) {
    rmat[r_ind[1,j],r_ind[2,j]] <- 1
    rmat[r_ind[2,j],r_ind[1,j]] <- 1
  }
  for(j in 1:N) {
    rmat[j,j] <- 1
  }
  r_matrices <- c(r_matrices, rmat)
}

invisible(r_matrices)
}

R_ext_Torrejonian<-make_random_matrices(MextTorrejonian,1000)