

source("Cell_info.R")


## intersection information table


NumInt <- NetRow * NetCol

IntID <- 1:(NetRow * NetCol)

Intersection <- data.frame(cbind((IntID) , merge(1:NetRow,1:NetCol) ))
colnames(Intersection) <- c("ID","Row","Col")
Intersection$ID <- as.character(Intersection$ID)


## Cell information & connectivity table


Cell    <- data.frame(Cell_info(m = NetRow , n = NetCol , a = main , b = end))
NumCell <- length(Cell$ID)

## intersection signal information table

IntSignalSetting <- data.frame(cbind(as.character(Intersection$ID),matrix(30 , nrow = NumInt , ncol = 4) , 0),stringsAsFactors = F)
colnames(IntSignalSetting) <- c("ID" , "EW_S", "EW_L", "NS_S","NS_L","Offset")
# IntSignalSetting$Offset <- c(0,10,20,30,40,50)

IntSignal  <- matrix(0 , nrow = NumInt , ncol = timestep)

for(int0 in 1:length(Intersection$ID)){
  signalset <- as.numeric(IntSignalSetting[int0,c("EW_S","EW_L","NS_S","NS_L")])
  offset    <- as.numeric(IntSignalSetting[int0,"Offset"])
  
  
  signal_length <-sum(signalset)
  
  signal_temp <- rep(signalset,ceiling( (total_time + offset) / signal_length))
  
  
  signal <- cumsum(signal_temp)
  signal <- signal - offset
  
  signal0 <- min(which(signal > 0))
  
  #' signal 1 = EW straight
  #' signal 2 = EW left turn
  #' signal 3 = NS straight
  #' signal 4 = NS left turn
  
  # if(signal0 == 1){
  #   signal0 <- 4
  # }else if(signal0 == 2){
  #   signal0 <- 1
  # }else if(signal0 == 3){
  #   signal0 <- 2
  # }else if(signal0 == 4){
  #   signal0 <- 3
  # }
  
  signalchange <- signal[which(signal > 0  & signal <= total_time) ]
  signalchange1 <- signalchange / dt
  
  cur_signal <- signal0
  for(k in 1:length(signalchange1)){
    
    if(k == 1){
      t000 <- 1
    }else{
      t000 <- signalchange1[k-1]
    }
    
    if( k == length(signalchange1)){
      t111 <- timestep
    }else{
      t111 <- (signalchange1[k] - 1)
    }
    
    
    
    
    IntSignal[int0, t000:t111  ] <- cur_signal
    
    if(cur_signal == 1){
      cur_signal <- 2
    }else if(cur_signal == 2){
      cur_signal <- 3
    }else if(cur_signal == 3){
      cur_signal <- 4
    }else if(cur_signal == 4){
      cur_signal <- 1
    }
  }
  
  
}



## intersection demand fraction table

DemandFrac <- matrix(0 , nrow = NumCell , ncol = 3 ,byrow = T)
IntDemandFrac <- data.frame(cbind( Cell$ID , DemandFrac), stringsAsFactors = F)
colnames(IntDemandFrac) <- c("ID","Frac_S","Frac_L","Frac_R")

# default <- c(1,0,0)
default <- c(0.7,0.2,0.1)

for(int0 in 1:length(IntDemandFrac$ID)){
  
  id <- IntDemandFrac$ID[int0]
  
  
  s_cell <- Cell$S_out[which(Cell$ID == id)]
  l_cell <- Cell$L_out[which(Cell$ID == id)]
  r_cell <- Cell$R_out[which(Cell$ID == id)]
  
  if(s_cell == 0){
    s_frac = 0
  }else{
    s_frac = default[1]
  }
  
  
  if(l_cell == 0){
    l_frac = 0
  }else{
    l_frac = default[2]
  }
  
  
  if(r_cell == 0){
    r_frac = 0
  }else{
    r_frac = default[3]
  }
  
  sum <- sum(s_frac , l_frac , r_frac)
  
  IntDemandFrac$Frac_S[int0] <- s_frac / sum
  IntDemandFrac$Frac_L[int0] <- l_frac / sum
  IntDemandFrac$Frac_R[int0] <- r_frac / sum
  
  
}

