
Cell_relation <-function(O_cell , D_cell){
  
  if ( D_cell == Cell$S_out[which(Cell$ID == O_cell)]){
    return(1)
  }else if ( D_cell == Cell$L_out[which(Cell$ID == O_cell)]){
    return(2)
  }else if ( D_cell == Cell$R_out[which(Cell$ID == O_cell)]){
    return(3)
  }else {
    return(0)
  }
  
}


update_veh_data <- function(O_cell , D_cell , vehid , time){
  if(Veh_data$CurCell[which(Veh_data$ID == vehid)]  != O_cell){
    print("error in update_veh_Data")
  }else{
    
    cur_cell    <<- D_cell
    
    
    if(D_cell %in% SinkCell$ID){
      next_cell <- 0
      Veh_data$NextCell[which(Veh_data$ID == vehid)]   <<- next_cell
      direction <- "N"
    }else{
      next_cell <- Veh_list[[as.numeric(vehid)]]$route[which(Veh_list[[as.numeric(vehid)]]$route  == D_cell)+1]
      Veh_data$NextCell[which(Veh_data$ID == vehid)]   <<- next_cell
      
      if(Cell$S_out[which(Cell$ID == D_cell)] == next_cell){
        direction <- "S"
      }else if(Cell$L_out[which(Cell$ID == D_cell)] == next_cell){
        direction <- "L"
      }else if(Cell$R_out[which(Cell$ID == D_cell)] == next_cell){
        direction <- "R"
      }
    }
    
    result <- c(vehid , cur_cell , next_cell , direction , time)  
    return(result)
  }
  
  
}


Generate.Vehicle <- function(vehid , Origin , Destination, time){
  
  paths <- all_shortest_paths(graph = graph , from = Origin , to = Destination )
  
  route0 <- paths$res[[1]]
  traveltime0 <- Inf
  
  for(k in 1:length(paths$res)){
    cell_path <- paths$res[[k]]
    
    speed <- v[cell_path,time]
    timespent <- unlist(lapply(speed , FUN = function(x){
      if(x == 0){
        return(1000000000)
      }else{
        return(CellLength/x)
      }  
    }   
    ))
    
    traveltime <- sum(timespent)
    
    if(traveltime < traveltime0){
      route0 <- cell_path
      traveltime0 = traveltime
    }
    
  }
  
  
  
  result0 <- list(ID = vehid , O_Cell = Origin , D_Cell = Destination , route = route0 , agressive = 0 , Veh_type = 0 , AV = 0)
  
  Veh_list[[vehid]] <<- result0
  
  
  if(  Cell_relation(route0[1] ,route0[2])  == 1){
    
    Cell_list[[route0[1]]]$Straight <<- append(Cell_list[[route0[1]]]$Straight , vehid)
    
  }else if(  Cell_relation(route0[1] ,route0[2])  == 2){
    
    Cell_list[[route0[1]]]$Left <<- append(Cell_list[[route0[1]]]$Left , vehid)
    
  }else if(  Cell_relation(route0[1] ,route0[2])  == 3){
    
    Cell_list[[route0[1]]]$Right <<- append(Cell_list[[route0[1]]]$Right , vehid)
    
  }
  
  
  n[route0[1],time] <<- n[route0[1],time] + 1
  
  # veh_result <- c(vehid, route0[2], route0[3] , "S", time)
  # return(veh_result) 
  
}
# 
# 
# O_cell <- 119
# D_cell <- 120
# inflow_input <- 1
# time <- 3



Update_Veh <- function(O_cell, D_cell , inflow_input , time){
  
  dir0 <- Cell_relation(O_cell , D_cell)
  
  prob0 <- inflow_input - floor(inflow_input)
  prob1 <- runif(1)
  if(prob0 < prob1){
    inflow_input <- floor(inflow_input)
  }else{
    inflow_input <- ceiling(inflow_input)
  }
  
  
  if(inflow_input != 0){
    
    
    
    
    veh_set <- c()
    
    if(dir0 == 1){
      
      inflow_input1  <- min(inflow_input  , length(Cell_list[[O_cell]]$Straight)-1  )
      
      if(inflow_input1 > 0){
        veh_set <- Cell_list[[O_cell]]$Straight[(2:(inflow_input1+1))]
        Cell_list[[O_cell]]$Straight <<-     Cell_list[[O_cell]]$Straight[-(2:(inflow_input1+1))]
      }
      
      
    }else if(dir0 == 2){
      
      inflow_input1  <- min(inflow_input  , length(Cell_list[[O_cell]]$Left)-1  )
      
      if(inflow_input1 > 0){
        veh_set <- Cell_list[[O_cell]]$Left[(2:(inflow_input1+1))]
        Cell_list[[O_cell]]$Left     <<-     Cell_list[[O_cell]]$Left[-(2:(inflow_input1+1))]
      }
      
    }else if(dir0 == 3){
      
      inflow_input1  <- min(inflow_input  , length(Cell_list[[O_cell]]$Right)-1  )
      
      if(inflow_input1 > 0){
        veh_set <- Cell_list[[O_cell]]$Right[(2:(inflow_input1+1))]
        Cell_list[[O_cell]]$Right     <<-     Cell_list[[O_cell]]$Right[-(2:(inflow_input1+1))]
      }
      
    }else{
      print("error in update veh")
    }
    
    if(length(veh_set) > 0){
      for(veh0 in 1:length(veh_set)){
        
        vehid <- veh_set[veh0]
        
        # if(vehid == 1){
        #   print(1)
        # }
        
        route0 <- Veh_list[[vehid]]$route
        
        loc <- which(route0 == D_cell)
        
        if(length(route0) == loc){
          
          Cell_list[[D_cell]]$Out <<- append(Cell_list[[D_cell]]$Out , vehid)
          
        }else{
          
          # dir1 <- Cell_relation(D_cell , next_cell)
          # 
          # 
          # if(dir1 == 1){
          #   
          #   Cell_list[[next_cell]]$Straight <<- append(Cell_list[[next_cell]]$Straight , vehid)
          #   
          # }else if(dir1 == 2){
          #   
          #   Cell_list[[next_cell]]$Left <<- append(Cell_list[[next_cell]]$Left , vehid)
          #   
          # }else if(dir1 == 3){
          #   
          #   Cell_list[[next_cell]]$Right <<- append(Cell_list[[next_cell]]$Right , vehid)
          #   
          # }else{
          #   print("error in update veh")
          # }
          
          Cell_list[[D_cell]]$Wait <<- append(Cell_list[[D_cell]]$Wait , vehid)
          
        }
        
      }
    }
    
    
    
    
    # print(1)
    # for(veh1 in 1:length(veh_set))
  }
  
  
  
  
  
} 







plot_network <- function(time0 ){
  
  tempsegment <- segment
  
  for(i in 1:length(tempsegment$ID)){
    cellid0 <- tempsegment$ID[i]
    
    tempsegment$nresult[i] <- n_agent[cellid0,time0]
    tempsegment$vresult[i] <- v[cellid0,time0]
  }
  
  tempIntersection <- Intersection
  
  tempIntersection$signal <- factor(1)
  levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")  
  
  for(i in 1:length(tempIntersection$ID)){
    
    sig <- IntSignal[as.numeric(Intersection$ID[i]),time0]
    
    if(sig == 1){
      tempIntersection$signal[i] <-   "EW_S"
    }else     if(sig == 2){
      tempIntersection$signal[i] <-   "EW_L"
    }else     if(sig == 3){
      tempIntersection$signal[i] <-   "NS_S"
    }else     if(sig == 4){
      tempIntersection$signal[i] <-   "NS_L"
    }
    
  }
  tempIntersection$signal <- as.factor(tempIntersection$signal)
  
  # levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")
  plot0 <-
    ggplot() + geom_point(data = tempIntersection, aes(x = x , y =y , fill = signal   ) ,  shape = 21 , colour = "black" , size = 5 ) + scale_fill_discrete( name = "Signal" , drop = F ) +
    geom_segment(data = tempsegment[which(!(tempsegment$ID %in% DemandCell$ID )& !(tempsegment$ID %in% SinkCell$ID)),] , aes( x=x0,y=y0,xend=x1,yend=y1,size = nresult , colour = vresult) , arrow = arrow(length=unit(0.2, "cm"))  ) + 
    scale_size_continuous(name = "line",breaks = c(seq(0, ceiling(CellLength * kjam /1000) , 1 ) , Inf)  , range = c(0.1,3) , guide = guide_legend(title = "Number of Veh" ), limits = c(0,max(as.vector(n))*1.5 )) + 
    scale_colour_gradient2(low = "red",mid = "yellow",high = "green", midpoint = vf/2 ,limits = c(0,vf) , "Speed")+
    geom_text(data= tempsegment , aes(x=(x1+x0)/2,
                                      y=(y1+y0)/2,
                                      label=round(nresult,2) ,
                                      angle= 90 * (Dir %in% c(1,3)) ) , color = "yellow" ,size = 3 ,fontface ="bold",check_overlap = T ) +
    geom_text(data= tempsegment , aes(x=(x1+x0)/2,
                                      y=(y1+y0)/2,
                                      label=round(nresult,2) ,
                                      angle= 90 * (Dir %in% c(1,3)) ) , color = "red" ,size = 3 ,check_overlap = T ) +
    scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+ 
    scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height))  + labs(title=paste0("time = ",time0))
  
  print(plot0)
  # ggsave(paste0(savepath,"Plot_agent/time_",time0,".png"),plot0 , width= width/CellLength + 1 , height= height/CellLength)
  
}
