
Get_CellMaxVeh <- function(i){
  
  length <- Cell$Length[which(Cell$ID == i)]
  
  maxveh <- length/1000 * kjam
  
  # if(i %in% SinkCell$ID){
  #   return(100000)
  # }else{
  return(maxveh)
  # }
  
  
}

get_v <- function(i,t){
  if(i > NumCell){
    return(vf)
  }
  return(v[i,t])
}

get_n <- function(i,t){
  # return(n[i,t])
  
  return(n_agent[i,t])
}

Q <- function(i){
  
  # if(i %in% SinkCell$ID){
  #   return(10000)
  # }else{
  return(qmax /3600 * dt)
  # }
  
  
}

update_v <- function(k){
  
  if(0 <= k  & k < kfree){
    updated_v <- vf
  }else if(k < kcap){
    
    updated_v <- qmax / k
    
  }else if(k <= kjam){
    
    q <- qmax - (k-kcap)*w
    
    updated_v <- q/k
    
  }else{
    updated_v <- 0
  }
  return(updated_v)
}

get_inflow <- function(i,j){
  inflow <- update_cell[which(update_cell[,1] == i & update_cell[,2] == j) , 4]
  
  if(length(inflow) == 0 ){
    return(0)
  }else{
    return(inflow)
  }
}

update_n  <- function(i,time){
  n0 <- get_n(i,time-1)
  
  
  inflow0  <- 0
  outflow0 <- 0
  
  id <- which(Cell$ID == i)
  
  incell  <- c( Cell$S_in[id]  , Cell$L_in[id]   , Cell$R_in[id]   )
  outcell <- c( Cell$S_out[id] , Cell$L_out[id]  , Cell$R_out[id] )
  
  for(in0 in 1:length(incell)){
    #' in0  = 1 : Straight
    #' in0  = 2 : Left turn
    #' in0  = 3 : Right turn
    
    # inflow_input <- inflow(i,time)[in0]
    
    inflow_input <- get_inflow(incell[in0] , i )
    
    if(length(inflow_input) == 0){
      inflow_input = 0
    }
    
    inflow0 <- inflow0   +  inflow_input
    
    
    # if(incell[in0] !=0){
    #   Update_Veh(incell[in0] , i , inflow_input , time)
    # }
    
    
    
  }
  
  for(out0 in 1:length(outcell)){
    #' out0  = 1 : Straight
    #' out0  = 2 : Left turn
    #' out0  = 3 : Right turn
    
    
    # outflow0 <- outflow0  + inflow(outcell[out0], t)[out0]
    inflow_input <- get_inflow(  i , outcell[out0] )
    
    if(length(inflow_input) == 0){
      inflow_input = 0
    }
    
    
    outflow0 <- outflow0  + inflow_input
    
    # if(outcell[out0] != 0){
    #   Update_Veh(i ,  outcell[out0] , inflow_input , time)
    # }
    
  }
  
  
  # inflow   <- inflow0
  # outflow  <- yout[i,time]
  
  nresult <- n0 + inflow0 - outflow0
  
  # for(i0 in 1:length(update_cell[,1])){
  #   O_cell <- update_cell[i0,1]
  #   D_cell <- update_cell[i0,2]
  #   inflow0  <- update_cell[i0,4]
  #   
  #   Update_Veh(O_cell , D_cell , inflow0, time)
  #   # Update_Veh(O_cell ,D_cell ,inflow_input = inflow0 , time , Veh_data0)
  # }
  
  
  ## Cell Agent Update
  ##
  ##
  ##
  
  yin[i,time] <<- inflow0
  
  if(is.na(nresult )){
    print(1)
  }
  
  return(nresult)
}

inflow <- function(RCell , time , Veh_data0){
  
  if(RCell == 0){
    return(c(0,0,0))
  }
  
  id <- which(Cell$ID == RCell)
  incell <- c( Cell$S_in[id] , Cell$L_in[id]  , Cell$R_in[id] )
  
  flowdemand <- c(0,0,0)
  
  for(m in 1:length(incell)){
    
    i <- incell[m]
    
    if(i != 0){
      
      if(xor(Cell$NetIn[i]  == 1 , Cell$NetOut[i] == 1 )){
        coef0 <- 1
      }else{
        coef0 <- minLength / Cell$Length[which(Cell$ID == i)]
      }
      
      if(m == 1){
        # straight direction
        
        
        
        
        int0 <- Cell$Int[which(Cell$ID == i)]
        id0 <- which(Cell$ID == i)
        
        loc0 <- Cell$Loc[id0]
        dir0 <- Cell$Dir[id0]
        
        
        if(int0 == 0){
          signal = 0
        }else{
          
          if(loc0 == dir0){
            signal <- 0
          }else{
            signal <-IntSignal[int0,time]
          }
          
          
        }
        
        
        Num_S <- length( Cell_list[[i]]$Straight) - 1
        Num_L <- length( Cell_list[[i]]$Left) - 1
        Num_R <- length( Cell_list[[i]]$Right) - 1
        
        
        S_coef <- (S_coef0 * (Num_S != 0) )/(( S_coef0 * (Num_S != 0) ) + (L_coef0 * (Num_L != 0) ) + (R_coef0 * (Num_R != 0) ) )
        if(is.nan(S_coef)){
          S_coef <- 1
        }
        
        
        
        # frac <- IntDemandFrac$Frac_S[which(IntDemandFrac$ID == i)]
        total_n <- sum( c( Num_S , Num_L , Num_R  ) )
        if(total_n == 0){
          frac <- 0
        }else{
          frac <- Num_S  / total_n
        }
        
        
        if(signal == 0){
          value <- coef0 * min(  get_n(i,time) * frac , Q(i) * S_coef  )
        }else{
          
          
          
          if(signal == 1){
            
            if( loc0 == 1 & dir0 == 3 ){
              signal = 0
            }else if( loc0 == 2 & dir0 == 4 ){
              signal = 1
            }else if( loc0 == 3 & dir0 == 1 ){
              signal = 0
            }else if( loc0 == 4 & dir0 == 2 ){
              signal = 1
            }else {
              signal = 0
            }
            
          }else if(signal == 2){
            
            signal = 0 
            
          }else if(signal == 3){
            
            if( loc0 == 1 & dir0 == 3 ){
              signal = 1
            }else if( loc0 == 2 & dir0 == 4 ){
              signal = 0
            }else if( loc0 == 3 & dir0 == 1 ){
              signal = 1
            }else if( loc0 == 4 & dir0 == 2 ){
              signal = 0
            }else {
              signal = 0
            }
            
          }else if(signal == 4){
            
            signal = 0
            
          }
          
          
          
          
          if(signal == 1){
            value <- coef0 * min(  get_n(i,time) * frac , Q(i)  * S_coef  )
          }else{
            value <- 0 
          }
        }
        
        
        
      }else if(m == 2){
        #left turn
        
        
        int0 <- Cell$Int[which(Cell$ID == i)]
        
        if(int0 == 0){
          signal = 0
        }else{
          signal <-IntSignal[int0,time]
        }
        
        
        # frac <- IntDemandFrac$Frac_L[which(IntDemandFrac$ID == i)]
        
        
        Num_S <- length( Cell_list[[i]]$Straight) - 1
        Num_L <- length( Cell_list[[i]]$Left) - 1
        Num_R <- length( Cell_list[[i]]$Right) - 1
        
        
        L_coef <- (L_coef0 * (Num_L != 0) )/(( S_coef0 * (Num_S != 0) ) + (L_coef0 * (Num_L != 0) ) + (R_coef0 * (Num_R != 0) ) )
        if(is.nan(L_coef)){
          L_coef <- 1
        }
        
        # frac <- IntDemandFrac$Frac_S[which(IntDemandFrac$ID == i)]
        total_n <- sum( c( Num_S , Num_L , Num_R  ) )
        if(total_n == 0){
          frac <- 0
        }else{
          frac <- Num_L  / total_n
        }
        
        
        # 
        # total_n <- sum( length( Cell_list[[i]]$Straight) - 1,  length( Cell_list[[i]]$Left) - 1   ,  length( Cell_list[[i]]$Right) - 1  )
        # if(total_n == 0){
        #   frac <- 0
        # }else{
        #   frac <- (length( Cell_list[[i]]$Left) - 1)   / total_n
        # }        
        
        
        # value <- coef0 * min(  get_n(i,time) * frac , Q(i)  )
        if(signal == 0){
          value <- 0
        }else{
          
          id0 <- which(Cell$ID == i)
          
          loc0 <- Cell$Loc[id0]
          dir0 <- Cell$Dir[id0]
          
          if(signal == 1){
            
            signal = 0
            
          }else if(signal == 2){
            
            if( loc0 == 1 & dir0 == 3 ){
              signal = 0
            }else if( loc0 == 2 & dir0 == 4 ){
              signal = 1
            }else if( loc0 == 3 & dir0 == 1 ){
              signal = 0
            }else if( loc0 == 4 & dir0 == 2 ){
              signal = 1
            }else {
              signal = 0
            }
            
          }else if(signal == 3){
            
            signal = 0
            
          }else if(signal == 4){
            
            if( loc0 == 1 & dir0 == 3 ){
              signal = 1
            }else if( loc0 == 2 & dir0 == 4 ){
              signal = 0
            }else if( loc0 == 3 & dir0 == 1 ){
              signal = 1
            }else if( loc0 == 4 & dir0 == 2 ){
              signal = 0
            }else {
              signal = 0
            }            
            
          }
          
          
          
          
          
          if(signal == 1){
            value <- coef0 * min(  get_n(i,time) * frac , min(  Q(i)/lane , Q(i) * L_coef  )  )
          }else{
            value <- 0 
          }
          
          
        }
        
      }else if(m == 3){
        #right turn
        
        # frac <- IntDemandFrac$Frac_R[which(IntDemandFrac$ID == i)]
        Num_S <- length( Cell_list[[i]]$Straight) - 1
        Num_L <- length( Cell_list[[i]]$Left) - 1
        Num_R <- length( Cell_list[[i]]$Right) - 1
        
        
        R_coef <- (R_coef0 * (Num_R != 0) )/(( S_coef0 * (Num_S != 0) ) + (L_coef0 * (Num_L != 0) ) + (R_coef0 * (Num_R != 0) ) )
        if(is.nan(R_coef)){
          R_coef <- 1
        }
        
        # frac <- IntDemandFrac$Frac_S[which(IntDemandFrac$ID == i)]
        total_n <- sum( c( Num_S , Num_L , Num_R  ) )
        if(total_n == 0){
          frac <- 0
        }else{
          frac <- Num_R  / total_n
        }
        
        
        
        
        value <-  coef0 * min(  get_n(i,time) * frac , min(Q(i)/3 , Q(i) * R_coef)  )
        
      }
      
    }else{
      value <- 0
    }
    
    # 
    # if(value < 0){
    #   print(1)
    # }
    
    
    flowdemand[m] <- value
    
  }
  
  if(xor(Cell$NetIn[RCell]  == 1 , Cell$NetOut[RCell] == 1 )){
    coef1 <- 1
  }else{
    coef1 <- minLength / Cell$Length[which(Cell$ID == RCell)] 
  }
  
  Receivable <-  coef1  *  max(0 ,   min(  Q(RCell),  w/vf * (Get_CellMaxVeh(RCell) - get_n(RCell,time)  )  )   ) 
  
  sum_flowdemand <-sum(flowdemand)
  if(sum_flowdemand == 0){
    f0 <- 0
  }else{
    f0 <- min( 1 ,  Receivable / sum(flowdemand) )
  }
  
  flowresult <- flowdemand * f0
  
  return(flowresult)
  
}

get_cell_path <- function(node_path){
  
  O_node <- as.numeric(node_path[1])
  D_node <- as.numeric(node_path[length(node_path)])
  
  
  O_cell <- DemandCell$ID[which(DemandCell$Node == O_node)]
  D_cell <- SinkCell$ID[which(SinkCell$Node == D_node)]
  
  
  # cur_node <- O_node
  next_cell <- Cell$S_out[which(Cell$ID == O_cell)]
  cell_path <- c(O_cell , next_cell)
  
  while(next_cell != D_cell){
    int0 <- Cell$Int[which(Cell$ID == next_cell)]
    if(int0 == 0){
      next_cell <- Cell$S_out[which(Cell$ID == next_cell)]
      cell_path <- append(cell_path , next_cell)
    }else{
      
      cur_node  <- int0
      next_node <- as.numeric(node_path[which(node_path == int0) + 1])
      
      if(next_node %in% Intersection$ID){
        row0 <- Intersection$Row[which(Intersection$ID == cur_node)]
        col0 <- Intersection$Col[which(Intersection$ID == cur_node)]
        
        row1 <- Intersection$Row[which(Intersection$ID == next_node)]
        col1 <- Intersection$Col[which(Intersection$ID == next_node)]
        
        
        
        if(row0 == row1){
          
          if(col0 > col1){
            direction <- 2
          }else{
            direction <- 4
          }
          
        }else if(col0 == col1){
          
          if(row0 > row1){
            direction <- 1
          }else{
            direction <- 3
          }
          
        }
        
        
        cur_dir <- Cell$Dir[which(Cell$ID == next_cell)]
        
        if(cur_dir == direction){
          next_cell <- Cell$S_out[which(Cell$ID == next_cell)]
          cell_path <- append(cell_path , next_cell)
        }else{
          
          if(cur_dir == 1){
            if(direction == 2){
              next_cell <- Cell$L_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }else if(direction == 4){
              next_cell <- Cell$R_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }
          }else if(cur_dir == 2){
            if(direction == 3){
              next_cell <- Cell$L_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }else if(direction == 1){
              next_cell <- Cell$R_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }
          }else if(cur_dir == 3){
            if(direction == 4){
              next_cell <- Cell$L_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }else if(direction == 2){
              next_cell <- Cell$R_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }
          }else if(cur_dir == 4){
            if(direction == 1){
              next_cell <- Cell$L_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }else if(direction == 3){
              next_cell <- Cell$R_out[which(Cell$ID == next_cell)]
              cell_path <- append(cell_path , next_cell)
            }
          }
          
          
          
        }
        
      }else{
        break
      }
    }
  }
  
  D_cell_path <- c(D_cell)
  cur_cell <- D_cell
  int0 <- Cell$Int[which(Cell$ID == cur_cell)]
  while(cur_node != int0){
    cur_cell <- Cell$S_in[which(Cell$ID == cur_cell)]
    D_cell_path <- append( cur_cell ,  D_cell_path )
    int0 <- Cell$Int[which(Cell$ID == cur_cell)]
  }
  
  cell_path <- append(cell_path , D_cell_path)
  return(cell_path)
}

update_y <- function(i,time){
  
  inflow0  <- 0
  outflow0 <- 0
  
  id <- which(Cell$ID == i)
  
  incell  <- c( Cell$S_in[id]  , Cell$L_in[id]   , Cell$R_in[id]   )
  outcell <- c( Cell$S_out[id] , Cell$L_out[id]  , Cell$R_out[id] )
  
  for(in0 in 1:length(incell)){
    #' in0  = 1 : Straight
    #' in0  = 2 : Left turn
    #' in0  = 3 : Right turn
    
    # inflow_input <- inflow(i,time)[in0]
    
    inflow_input <- update_cell[which(update_cell[,2] == i & update_cell[,3] == in0 ) , 4]
    
    if(length(inflow_input) == 0){
      inflow_input = 0
    }
    
    inflow0 <- inflow0   +  inflow_input
    
  }
  
  for(out0 in 1:length(outcell)){
    #' out0  = 1 : Straight
    #' out0  = 2 : Left turn
    #' out0  = 3 : Right turn
    
    
    # outflow0 <- outflow0  + inflow(outcell[out0], t)[out0]
    inflow_input <- update_cell[which(update_cell[,2] == outcell[out0] & update_cell[,3] == out0 ) , 4]
    
    if(length(inflow_input) == 0){
      inflow_input = 0
    }
    
    
    outflow0 <- outflow0  + inflow_input
  }
  
  yresult <- c(inflow0 , outflow0)
  
  if(is.na(inflow0 )){
    print(1)
  }
  if(is.na(outflow0 )){
    print(1)
  }
  
  
  return(yresult)
}

Sim_update <- function(time){
  
  ########################    1      ######################################
  ################################################################################
  veh_gen_set <- Veh_Q[which(Veh_Q$Time == time),]
  if( length(veh_gen_set$VehID) >= 1 ){
    for(veh in 1:length(veh_gen_set$VehID)){
      
      vehid   <- veh_gen_set$VehID[veh]
      origin  <- veh_gen_set$Origin[veh]
      destin  <- veh_gen_set$Destination[veh]
      
      
      Generate.Vehicle(vehid, origin, destin , time)
      
    }
  }
  ################################################################################
  
  
  if(time !=1){
    ########################    2      ######################################
    ################################################################################
    
    for(i in Cell$ID ){
      
      
      updated_n <-  update_n(i , time )
      n[i,time] <<- updated_n
      
      
    }
    
    ########################    3      ######################################
    ################################################################################
    
    for(i0 in 1:length(update_cell[,1])){
      O_cell <- update_cell[i0,1]
      D_cell <- update_cell[i0,2]
      inflow0  <- update_cell[i0,4]
      
      Update_Veh(O_cell , D_cell , inflow0, time)
      # Update_Veh(O_cell ,D_cell ,inflow_input = inflow0 , time , Veh_data0)
    }
    
    ########################    4      ######################################
    ################################################################################
    
    for(i in Cell$ID){
      veh_wait <- Cell_list[[i]]$Wait
      
      if(length(veh_wait) > 1){
        
        veh_wait <- veh_wait[-1]
        
        for(veh0 in 1:length(veh_wait)){
          
          vehid <- veh_wait[veh0]
          route0 <- Veh_list[[vehid]]$route
          
          loc <- which(route0 == i)
          
          dir0 <- Cell_relation(i , route0[loc+1])
          
          
          if(dir0 == 1){
            Cell_list[[i]]$Wait     <<- Cell_list[[i]]$Wait[-which(Cell_list[[i]]$Wait == vehid)]
            Cell_list[[i]]$Straight <<- append(Cell_list[[i]]$Straight , vehid)
          }else if(dir0 == 2){
            Cell_list[[i]]$Wait     <<- Cell_list[[i]]$Wait[-which(Cell_list[[i]]$Wait == vehid)]
            Cell_list[[i]]$Left     <<- append(Cell_list[[i]]$Left , vehid)
          }else if(dir0 == 3){
            Cell_list[[i]]$Wait     <<- Cell_list[[i]]$Wait[-which(Cell_list[[i]]$Wait == vehid)]
            Cell_list[[i]]$Right    <<- append(Cell_list[[i]]$Right , vehid)
          }
          
          
          
        }
        
        
      }
      
      n_agent[i,time] <<- sum( length( Cell_list[[i]]$Straight) - 1,  length( Cell_list[[i]]$Left) - 1   ,  length( Cell_list[[i]]$Right) - 1  )
      
    }
    
    
    for(i in Cell$ID){
      k <- get_n(i,time) / (Cell$Length[i] / 1000)
      
      updated_v <- update_v(k)
      
      v[i,time] <<- updated_v
    } 
  }
  saveRDS(Cell_list , paste0(savepath , "Cell_list/", "Cell",time ,".RDS"))
  
  # saveRDS(object = Veh_data , file = paste0(savepath , "Vehicle" , "/Veh_data_",time,".RDS"))
  
  
  if(time != timestep){
    
    
    # Veh_data0 <-Veh_data
    ########################    6      ######################################
    ################################################################################
    
    
    for(i0 in 1:length(update_cell[,1])){
      O_cell <- update_cell[i0,1]
      D_cell <- update_cell[i0,2]
      direction <- update_cell[i0,3]
      
      inflow0  <- inflow(D_cell, time , Veh_data0)[direction]
      
      if(is.nan(inflow0)){
        print(1)
      }
      
      if(is.na(inflow0)){
        print(1)
      }
      
      update_cell[i0,4] <<- inflow0
      
      # Update_Veh(O_cell ,D_cell ,inflow_input = inflow0 , time , Veh_data0)
    }
    
    
    saveRDS(update_cell , paste0(savepath , "update_cell/", "Update",time ,".RDS"))
    # for(i in Cell$ID    ){
    #   yresult <-  update_y( i , time )
    #   
    #   yin[i  , time + 1]  <<- yresult[1]
    #   yout[i , time + 1]  <<- yresult[2]
    # }
    
    
    
  }
  
}