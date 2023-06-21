#### prepare data set for simulation ####
# requires relfreq_35

# set seed
set.seed(42)

N <- 8000 # originally 500000
M <- 1
P <- relfreq_35$relfreq

# based on gendist.m - originally by Tristan Ursell
Pnorm <- c(0,P)/sum(P)
Pcum <- cumsum(Pnorm)

# create random matrix
R <- matrix(runif(N*M), nrow = 1, ncol = N*M)
T <- as.vector(histc(R, Pcum)$bin)

rho <- T-1

## estimated parameters
lambda1_hat <- 0.89
lambda2_hat <- 0.0000629
lambda3_hat <- 0.000988

## Share that acts as if there are no cost (z = 0)
z_temp <- rep(0,N)
nozero <- (rho != 0)
nz <- sum(nozero)

## take random numbers from uniform distribution between 0-1
## above lambda1 -> cost
z_temp[nozero] <- (runif(nz) > lambda1_hat)
nd <- sum(z_temp)
z <- (z_temp == 1)


## draw deltas for individuals with z = 1
delta <- rep(0,N)
delta[z] <- rexp(nd, lambda2_hat)

## draw alphas
alpha <- rexp(N, lambda3_hat) 


## utility levels at different choice options
Vx <- (rho^2)/2   # V(rho,rho)
Ux <- Vx - delta  # V(rho,rho)-delta
U0 <- -alpha
Ud10 <- rho * 10 - (10^2)/2  # V(d,rho) for d = 10
Ud20 <- rho * 20 - (20^2)/2 # V(d,rho) for d = 20
Ud50 <- rho * 50 - (50^2)/2 # V(d,rho) for d = 50


## Simulation for default = 10

# individuals' optimal choice
sim_donation10 <- rep(0,N)


# if no deviation cost -> always pick rho
sim_donation10[!z] <- rho[!z]

# if endure deviation cost
switch_to_rho <- rep(0,N)
switch_to_rho[z] <- (Ux[z] >= Ud10[z] & Ux[z] >= U0[z]) 

stay_default <- rep(0,N)
stay_default[z] <- (Ud10[z] > Ux[z] & Ud10[z] >= U0[z])

opt_out <- rep(0,N)
opt_out[z] <- (U0[z] > Ux[z] & U0[z] > Ud10[z])


# assign donation to simulated donations
sim_donation10[switch_to_rho == 1] <- rho[switch_to_rho==1]
sim_donation10[stay_default == 1] <- 10
sim_donation10[opt_out == 1] <- 0


# save to data set
default10 <- rep(10,length(sim_donation10))
simulation10 <- rep("simulation10",length(sim_donation10))

simulation <- data.frame("default_donation" = default10, "donated_amount" = sim_donation10, "simulation" = simulation10)


## Simulation for default = 20

# individuals' optimal choice
sim_donation20 <- rep(0,N)


# if no deviation cost -> always pick rho
sim_donation20[!z] <- rho[!z]

# if endure deviation cost
switch_to_rho <- rep(0,N)
switch_to_rho[z] <- (Ux[z] >= Ud20[z] & Ux[z] >= U0[z])

stay_default <- rep(0,N)
stay_default[z] <- (Ud20[z] > Ux[z] & Ud20[z] >= U0[z])

opt_out <- rep(0,N)
opt_out[z] <- (U0[z] > Ux[z] & U0[z] > Ud20[z])


# assign donation to simulated donations
sim_donation20[switch_to_rho == 1] <- rho[switch_to_rho == 1]
sim_donation20[stay_default == 1] <- 20
sim_donation20[opt_out == 1] <- 0

# save to data set
default20 <- rep(20,length(sim_donation20))
simulation20 <- rep("simulation20",length(sim_donation20))

simulation <- rbind(simulation, data.frame("default_donation" = default20, "donated_amount" = sim_donation20, "simulation" = simulation20))


## Simulation for default = 50

# individuals' optimal choice
sim_donation50 <- rep(0,N)


# if no deviation cost -> always pick rho
sim_donation50[!z] <- rho[!z]

# if endure deviation cost
switch_to_rho <- rep(0,N)
switch_to_rho[z] <- (Ux[z] >= Ud50[z] & Ux[z] >= U0[z])

stay_default <- rep(0,N)
stay_default[z] <- (Ud50[z] >= Ux[z] & Ud50[z] >= U0[z])

opt_out <- rep(0,N)
opt_out[z] <- (U0[z] > Ux[z] & U0[z] > Ud50[z])


# assign donation to simulated donations
sim_donation50[switch_to_rho == 1] <- rho[switch_to_rho == 1]
sim_donation50[stay_default == 1] <- 50
sim_donation50[opt_out == 1] <- 0


# save to data set
default50 <- rep(50,length(sim_donation50))
simulation50 <- rep("simulation50",length(sim_donation50))

simulation <- rbind(simulation, data.frame("default_donation" = default50, "donated_amount" = sim_donation50, "simulation" = simulation50))


# fill in missing rows
n <- nrow(simulation) - nrow(section4_35)
section4_35obs <- rbind(section4_35, data.frame("default_donation" = rep(NA,n), "donated_amount" = rep(NA,n)))

# create complete data set
observations <- section4_35obs %>% mutate("simulation" = "observation")
data_complete <- rbind(simulation, observations)

# save data set
save(data_complete, file = "DefaultsAndDonations_data_complete")
