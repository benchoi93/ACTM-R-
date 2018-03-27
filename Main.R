# install.packages("igraph")
 
 
library(igraph)
library(ggplot2)
library(foreach)
library(doParallel)

VHTresult <- c(0)

# 
# cl <- makeCluster(3)
# registerDoParallel(cl, cores = 3)
# getDoParName()


# foreach(sim = 1:100 , .packages = c("igraph" , "ggplot2" , "foreach","doParallel")) %dopar% {
for(sim in 1:1){
  dir.create(paste0("result_",sim) , showWarnings = F)
  savepath <- paste0("result_",sim,"/")
  
  # dir.create(paste0("result_",sim) , showWarnings = F)
  # savepath <- paste0("result_",sim,"/")
  
  NetRow <- 4
  NetCol <- 4
  main   <- 3
  end    <- 4
  lane   <- 3
  
  source("Global.R")
  source("Functions.R")
  source("Veh.functions.R")
  
  DemandCell <- Cell[which(Cell$NetIn == 1),]
  SinkCell   <- Cell[which(Cell$NetOut == 1),]
  
  
  
  
  if(sim == 1){
    source("Network_Setting.R")
  }
  
  source("Cell list.R")
  
  # DemandCell <- DemandCell[order(DemandCell$Node),]
  # SinkCell <- SinkCell[order(SinkCell$Node),]
  
  OD_matrix <- matrix(0 , nrow = length(DemandCell$ID) , ncol = length(SinkCell$ID))
  
  for(k in 1:length(ODpair[,1])){
    OD_matrix[which(DemandCell$ID == ODpair[k,1]) ,which(SinkCell$ID == ODpair[k,2]) ] <- 1
  }
  
  target <- rbind(
    merge(c(1,18,35,52)  ,  c(17,34,51,68)),
    merge(c(188, 171, 154 , 107) , c( 204, 187, 170 , 153))
  )
  #Origin Cell , Destination  Cell
  
  multi <-  20
  for(tar in 1:length(target[,1])){
    OD_matrix[which(DemandCell$ID == target[tar,1]) ,which(SinkCell$ID == target[tar,2]) ] <- multi * OD_matrix[which(DemandCell$ID == target[tar,1]) ,which(SinkCell$ID == target[tar,2]) ]
  }
  
  low_demand0  <- 0.01
  test_demand0 <- low_demand0 * 10
  
  source("Gen_Demand.R")
  # Veh_Q <- readRDS("Veh_Q.RDS")
  
  
  current_time <- proc.time()
  # n[DemandCell$ID,] <- 3
  
  print("simulation start")
  # dir.create(paste0(savepath , "Vehicle" ) , showWarnings = F)
  
  
  dir.create(paste0(savepath, "Cell_list"))
  dir.create(paste0(savepath, "update_cell"))
  
  # veh_numbering <- 0 
  for(t in 1:(timestep)){
  # for(t in 1:(100)){

    Sim_update(time = t)
    # print(t)
    
  }
  
  
  timepass <- proc.time() - current_time
  print(timepass)
  print("simulation end")
  
  VHT <- sum(n) * dt
  print(VHT)
  VHTresult <- append(VHTresult , VHT)
  
  # if(sum(n[,timestep]) == 0 ){
    
    saveRDS(n , paste0(savepath, "nresult.RDS"))
    saveRDS(n_agent , paste0(savepath, "nagentresult.RDS"))
    saveRDS(v , paste0(savepath, "vresult.RDS"))
    saveRDS(yin , paste0(savepath, "yinresult.RDS"))
    saveRDS(Veh_Q , paste0(savepath, "Veh_Q.RDS"))
    saveRDS(Veh_list , paste0(savepath, "Veh_list.RDS"))
  # }
  
  # saveRDS(VHTresult , "VHTresult.RDS")
    
  source("plot.R")
  print("plot end")
}

# stopCluster(cl)
