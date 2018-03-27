
#warmup phase
timestep1 <- low_time * 60/dt

# low_demand0 # veh / 1 cell / 1 timestep


time_in <- 1

while(sum(time_in) < timestep1){
  time_in <- append(time_in, ( rexp(1, low_demand0*NumCell) )  )
}

time_set <- cumsum(time_in)

Veh_Q <- c()

for(veh in 1:length(time_set)){
  
  demand <- rowSums(OD_matrix) 
  demand <- demand/sum(demand) #Origin Probability
  
  origin <- DemandCell$ID[sample(x = 1:length(demand) , size = 1 , prob = demand)]
  time0  <- time_set[veh]
  
  thre0 <- time0 - floor(time0)
  prob0 <- runif(1)
  
  if(prob0 < thre0){
    time0 <- ceiling(time0)
  }else{
    time0 <- floor(time0)
  }
  
  
  Veh_Q <- rbind(Veh_Q , c(veh , origin , time0))
}
Veh_Q1 <- Veh_Q


#test phase
timestep2 <- test_time*60/dt

time_in <- 0

while(sum(time_in) < timestep2){
  time_in <- append(time_in, ( rexp(1, test_demand0 * NumCell) )  )
}

time_set <- cumsum(time_in)

Veh_Q <- c()

for(veh in 1:length(time_set)){
  
  demand <- rowSums(OD_matrix)
  demand <- demand/sum(demand)
  
  origin <- DemandCell$ID[sample(x = 1:length(demand) , size = 1 , prob = demand)]
  time0  <- time_set[veh]
  
  thre0 <- time0 - floor(time0)
  prob0 <- runif(1)
  
  if(prob0 < thre0){
    time0 <- ceiling(time0)
  }else{
    time0 <- floor(time0)
  }
  
  
  Veh_Q <- rbind(Veh_Q , c(veh + length(Veh_Q1[,1]), origin , time0 + timestep1))
}
Veh_Q2 <- Veh_Q




#unloading phase
timestep3 <- low_time2*60/dt


time_in <- 0

while(sum(time_in) < timestep3){
  time_in <- append(time_in, ( rexp(1, low_demand0 * NumCell) )  )
}

time_set <- cumsum(time_in)

Veh_Q <- c()

for(veh in 1:length(time_set)){
  
  demand <- rowSums(OD_matrix)
  demand <- demand/sum(demand)
  
  origin <- DemandCell$ID[sample(x = 1:length(demand) , size = 1 , prob = demand)]
  time0  <- time_set[veh]
  
  thre0 <- time0 - floor(time0)
  prob0 <- runif(1)
  
  if(prob0 < thre0){
    time0 <- ceiling(time0)
  }else{
    time0 <- floor(time0)
  }
  
  
  Veh_Q <- rbind(Veh_Q , c(veh + length(Veh_Q1[,1]) + length(Veh_Q2[,2]), origin , time0 + timestep1 + timestep2))
}
Veh_Q3 <- Veh_Q




Veh_Q <- rbind(Veh_Q1 , Veh_Q2 , Veh_Q3)





Veh_Q <- data.frame(Veh_Q)
colnames(Veh_Q) <- c("VehID" , "Origin", "Time")

Veh_Q <- cbind(Veh_Q , Destination = 0)


for(i in 1:length(Veh_Q$VehID)){
  incell <- Veh_Q$Origin[i]
  
  row0 <- which(DemandCell$ID == incell)
  
  OD_demand <- OD_matrix[row0,  ]
  OD_demand <- OD_demand/sum(OD_demand)
  
  Origin     <- DemandCell$ID[row0]
  Destination <- Origin
  
  while( Origin == Destination){
    col0 <- sample(x = 1:length(OD_demand) , size = 1 , prob = OD_demand)
    Destination <- SinkCell$ID[col0]
  }
  
  Veh_Q$Destination[i] <- Destination
}



