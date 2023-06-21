
# replication relfreq_varyingbin.m
relfreq <- function(donations){ 
  
  total <- nrow(donations)
  
  # create count
  count1 <- donations %>%
    count(donated_amount)
  
  count <- data.frame("donated_amount" = seq(0,300)) %>% 
    left_join(count1, by = "donated_amount") %>% 
    replace_na(list(n = 0))
  
  # create bandwidth
  one <- c((1:31), seq(36, 101, by = 5), seq(111,191, by = 10), 201, 251, 301)
  four <- c((32:35), (37:40), (42:45), (47:50), (52:55), (57:60), (62:65), (67:70), (72:75), (77:80), (82:85), (87:90), (92:95), (97:100))
  nine <- c((102:110), (112:120), (122:130), (132:140), (142:150), (152:160), (162:170), (172:180), (182:190), (192:200), (242:250), (292:300))
  ten <- c((202:241), (252:291))
  
  bandwith <- rep(0,301)
  
  bandwith[is.element(1:301, one)] = 1
  bandwith[is.element(1:301, four)] = 4
  bandwith[is.element(1:301, nine)] = 9
  bandwith[is.element(1:301, ten)] = 10
  
  # calculate count 
  count_bw <- count$n
  
  a1 <- seq(31, 96, by = 5)
  for (i in 1:length(a1)) {
    count_bw[(a1[i]+1):(a1[i]+4)] = sum(count_bw[(a1[i]+1):(a1[i]+4)])
  }
  
  a2 <- seq(101, 191, by = 10)
  for (i in 1:length(a2)) {
    count_bw[(a2[i]+1):(a2[i]+9)]= sum(count_bw[(a2[i]+1):(a2[i]+9)])
  }
  
  a3 <- seq(202, 292, by = 10)
  for (i in 1:length(a3)) {
    if (a3[i]!= 242 && a3[i] != 292) {
      count_bw[a3[i]:(a3[i]+9)] = sum(count_bw[a3[i]:(a3[i]+9)])
    } else {
      count_bw[a3[i]:(a3[i]+8)]= sum(count_bw[a3[i]:(a3[i]+8)])
    } 
  } 
  
  # (vi) calculate relcount and relfreq
  relcount <- count_bw/bandwith
  relfreq <- relcount/total
  
  df_relfreq <- cbind(count, as.data.frame(bandwith), as.data.frame(count_bw), as.data.frame(relcount), as.data.frame(relfreq))
  
  return(df_relfreq)
}



## replication totallikelihood.m
loglikelihood <- function(lambda, rho, delta, default, probmf, c1, c2){
  
  lambda1 <- lambda[1]
  lambda2 <- lambda[2]/c1 # incl. rescaling
  lambda3 <- lambda[3]/c2 # incl. rescaling
  
  lli <- rep(0,length(rho))
  
  # assign default values
  d1 <- 10
  d2 <- 20
  d3 <- 50
  
  # assign probability mass to each observed value of rho -> estimpmf(probmf, rho)
  f <- rep(0, length(rho))
  rho_r <- round(rho)
  
  for (i in 1:length(rho)) {
    f[i] <- probmf[rho_r[i]+1]
  }
  
  ## define Sum_lik0, this function calculates the sum in the probability for case 1
  Sum_lik0 <- function(lambda2, lambda3, d, probmf){
    # sum over rho, 0 < rho < d/2
    r <- (1:(d/2 -1))
    def <- d * rep(1, length(r))
    # probability mass function: elements correspond to rho-1 
    pmass <- probmf[2:(d/2)]
    
    Del <- ((r^2) + (def^2))/2 - r * def
    Sum0_i <- pmass * ((lambda3/(lambda2+lambda3)) * exp(-lambda2*(r^2)/2)-
                         (lambda3/(lambda2+lambda3)) * exp(-lambda3*((def^2)/2 - r * def)) * exp(-lambda2 * Del))
    
    return(sum(Sum0_i))
  }
  
  ## define Sum_likd, this function calculates the sum in the probability for case 2
  Sum_likd <- function(lambda2, lambda3, d, probmf){
    # rho from 1 to 300
    r <- 1:300
    def <- d * rep(1, length(r))
    # probability mass function: elements correspond to rho-1 
    pmass <- probmf[2:301]
    
    Sumd_i <- rep(0, length(r))
    Del <- ((r^2)+(def^2))/2 - r*(def)
    # sum for rho <= d/2
    a <- r[(r<=(d/2))]
    # sum for rho > d/2 & rho!=d
    b <- r[(r>(d/2) & r!=d)]
    
    Sumd_i[a] <- pmass[a]*exp(-lambda3*((def[a]^2)/2 - r[a]*def[a]))*exp(-lambda2*Del[a])
    Sumd_i[b] <- pmass[b]*exp(-lambda2*Del[b])
    
    return(sum(Sumd_i))
  }
  
  
  # calculate sums for x = 0 and x = d in probability
  Sum0_d1 <- Sum_lik0(lambda2, lambda3, d1, probmf)
  Sum0_d2 <- Sum_lik0(lambda2, lambda3, d2, probmf)
  Sum0_d3 <- Sum_lik0(lambda2, lambda3, d3, probmf)
  
  Sumd_d1 <- Sum_likd(lambda2, lambda3, d1, probmf)
  Sumd_d2 <- Sum_likd(lambda2, lambda3, d2, probmf)
  Sumd_d3 <- Sum_likd(lambda2, lambda3, d3, probmf)
  
  
  # log likelihood for each observation i up to N; define cases as in Appendix section D
  # case 1: x = 0
  case1_d1 <- (default == d1 & rho == 0)
  case1_d2 <- (default == d2 & rho == 0)
  case1_d3 <- (default == d3 & rho == 0)
  # case 2: x = d
  case2_d1 <- (default == d1 & rho == d1)
  case2_d2 <- (default == d2 & rho == d2)
  case2_d3 <- (default == d3 & rho == d3)
  # case 3: 0 < x < d/2 
  # note: in the appendix case 3 comprises both case 3 and 4 in code
  case3_d1 <- (default == d1 & rho < d1/2 & rho > 0)
  case3_d2 <- (default == d2 & rho < d2/2 & rho > 0)
  case3_d3 <- (default == d3 & rho < d3/2 & rho > 0)
  # case 4: x >= d/2, x != d
  case4_d1 <- (default == d1 & rho >= d1/2 & rho != d1)
  case4_d2 <- (default == d2 & rho >= d2/2 & rho != d2)
  case4_d3 <- (default == d3 & rho >= d3/2 & rho != d3)
  
  
  # case 1 individual log likelihood
  lli[case1_d1] <- log(f[case1_d1] + (1-lambda1) * Sum0_d1)
  lli[case1_d2] <- log(f[case1_d2] + (1-lambda1) * Sum0_d2)
  lli[case1_d3] <- log(f[case1_d3] + (1-lambda1) * Sum0_d3)
  
  # case 2 individual log likelihood
  lli[case2_d1] <- log(f[case2_d1] + (1-lambda1) * Sumd_d1)
  lli[case2_d2] <- log(f[case2_d2] + (1-lambda1) * Sumd_d2)
  lli[case2_d3] <- log(f[case2_d3] + (1-lambda1) * Sumd_d3)
  
  
  ## define case3_lik, this function calculates the second term of the probability for case 3
  case3_lik <- function(lambda1, lambda2, lambda3, default, delta, rho){
    
    # assign default values
    d1 <- 10
    d2 <- 20
    d3 <- 50
    
    # define case 3
    case3 <- ((default == d1 & rho < (d1/2) & rho > 0) | 
              (default == d2 & rho < (d2/2) & rho > 0) | 
              (default == d3 & rho < (d3/2) & rho > 0))
    
    # preallocate
    z <- rep(0, length(rho))
    a <- rep(0, length(rho))
    a[case3] <- -lambda2 * delta[case3] - lambda3 * (((default[case3])^2)/2-rho[case3] * default[case3])
    z[case3] <- lambda1 + (1-lambda1) * (1-exp(-lambda2 * ((rho[case3])^2)/2) + (lambda2/(lambda2+lambda3)) * (exp(-lambda2*((rho[case3]^2)/2) - exp(a[case3]))))
    
    return(z)
  }
  
  
  # case 3 individual log likelihood
  # let z be the second term of the probability
  z <- case3_lik(lambda1, lambda2, lambda3, default, delta, rho)
  
  lli[case3_d1] <- log(f[case3_d1]) + log(z[case3_d1])
  lli[case3_d2] <- log(f[case3_d2]) + log(z[case3_d2])
  lli[case3_d3] <- log(f[case3_d3]) + log(z[case3_d3])
  
  # case 4 individual log likelihood
  lli[case4_d1] <- log(f[case4_d1]) + log(lambda1 + (1-lambda1) * (1-exp(-lambda2 * delta[case4_d1])))
  lli[case4_d2] <- log(f[case4_d2]) + log(lambda1 + (1-lambda1) * (1-exp(-lambda2 * delta[case4_d2])))
  lli[case4_d3] <- log(f[case4_d3]) + log(lambda1 + (1-lambda1) * (1-exp(-lambda2 * delta[case4_d3])))
  
  
  # total log likelihood
  LL <- sum(lli)
  
  return(LL)
}