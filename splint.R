solve.splint = function(x){
  require(quadprog)
  require(Matrix)
  
  # check if x is numeric vector with length > 2
  
  k = length(x)
  
  # Objective function
  d = rep(0, 4*k)  # no linear terms
  D = matrix(0, 4*k, 4*k)  # quadratic terms
  for (i in 1:k){
    tmp = matrix(c(4, 12*(2*i-1), 12*(2*i-1), 12*(3*i^2 - 3*i + 1)), 2, 2)
    idx = (4*i-1):(4*i)
    D[idx,idx] = tmp
  }
  
  # Constraints (LHS)
  A = matrix(0, 4*k, 3*k-2)
  
  # Continuity
  for (i in 1:(k-1)){
    idx = (4*i-3):(4*(i+1))
    A[idx,i] = c(1, i, i^2, i^3, -1, -i, -i^2, -i^3)
  }
  
  # Differentiability (continuity of first derivative)
  for (i in 1:(k-1)){
    idx = (4*i - 3):(4*(i+1))
    A[idx,k-1+i] = c(0, 1, 2*i, 3*i^2, 0, -1, -2*i, -3*i^2)
  }
  
  # Temporal additivity
  for (i in 1:k){
    idx = (4*i - 3):(4*i)
    A[idx, 2*k-2+i] = c(1, (1/2)*(i^2-(i-1)^2), (1/3)*(i^3-(i-1)^3), (1/4)*(i^4-(i-1)^4))
  }
  
  # Constraints (RHS)
  b = rep(0, 3*k - 2)
  b[(2*k-1):(3*k-2)] = x
  
  # Find positive definite matrix nearest to D
  Dapprox = matrix(nearPD(D)$mat@x, 4*k, 4*k)
  
  # Return optimal coefficient vector
  solve.QP(Dapprox, d, A, b, meq=length(b))$solution
}

splint = function(x, f=3) {
  # solve
  k = length(x)
  coef = solve.splint(x)
  coef.mat = matrix(coef, 4, k)
  
  # Integrate to obtain interpolates
  x.int = rep(NA, k*f)
  for (i in 1:(k*f)){
    t1 = i/f
    t0 = (i-1)/f
    idx = (i-1) %/% f + 1
    x.int[i] = f*sum(coef.mat[,idx] * c(1/f, (1/2)*(t1^2 - t0^2), (1/3)*(t1^3 - t0^3), (1/4)*(t1^4 - t0^4)))
  }
  x.int
}

# # Test
# set.seed(45394)
# N = 200
# freq = 4
# x.orig = cumsum(rnorm(N, 0, 1))
# x.agg = rep(NA, N/freq)
# x.const = rep(x.agg, each=freq)
# for (i in 1:(N/freq)) x.agg[i] = mean(x.orig[((i-1)*freq+1):(i*freq)])
# 
# plot(x.orig, type="l")
# lines(x.const, col="red")
# 
# x.int = splint(x.agg, f=freq)
# plot(x.const, type="l")
# lines(x.int, col="red")
# 
# plot(x.orig, type="l")
# lines(x.int, col="red")
# lines(x.const, col="blue")
