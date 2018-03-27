
test_time <- 15
low_time  <- 5
low_time2  <- 10
low_time3  <- 0
total_time <- (low_time + test_time + low_time2 + low_time3 )   *  60 ## minute (seconds)



dt <- 5 ## seconds

total_time <- total_time #seconds
timestep <- total_time / dt + 1

if(sim == 1){
  source("Cell Setting.R")
}

#Simulation Parameters
vehlength <- 7         #m
vf   <- 50             #km/hr  == free flow speed
qmax <- 1800 * lane           #veh/hr/lane == capacity
w    <- 15             #km/hr (negative) == wave speed
# tau  <- 3              #sec  == delay to initiate a lane change
L_coef0 <- 1
R_coef0 <- 1
S_coef0 <- 3


# tau  <- 1              #sec  == delay to initiate a lane change

kjam  <- 1000/vehlength * lane #veh/km == jam density
kfree <- qmax/vf 
kcap  <- kjam - qmax/w

minLength  <- vf * dt / 3600 * 1000  #km
CellLength <- 100

n     <- matrix(data = 0     , nrow = NumCell , ncol = timestep)
yin   <- matrix(data = 0     , nrow = NumCell , ncol = timestep)
yout  <- matrix(data = 0     , nrow = NumCell , ncol = timestep)
v     <- matrix(data = vf    , nrow = NumCell , ncol = timestep)


n_agent     <- matrix(data = 0     , nrow = NumCell , ncol = timestep)
