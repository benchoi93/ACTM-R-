

library(ggplot2)
# 
# 

width  = ((NetCol - 1) * main + 2 * end ) * CellLength
height = ((NetRow - 1) * main + 2 * end ) * CellLength

for(m in 1:length(Intersection$ID)){
  row0 <- Intersection$Row[m]
  col0 <- Intersection$Col[m]
  
  x0 <- end*CellLength + (col0-1)*main*CellLength
  y0 <- height - end*CellLength - (row0-1)*main*CellLength
  
  Intersection$x[m] <- x0
  Intersection$y[m] <- y0
}


segment0 <- Cell[which(Cell$Int != 0),]

for(m in 1:length(segment0$ID)){
  Int <- segment0$Int[m]
  Loc <- segment0$Loc[m]
  Dir <- segment0$Dir[m]
  
  int_x0 <- Intersection$x[which(Intersection$ID == Int)]
  int_y0 <- Intersection$y[which(Intersection$ID == Int)]
  
  if(Loc == 1){
    if(Dir == 1){
      
      x0 <- int_x0 + CellLength/10
      y0 <- int_y0 + CellLength/10
      
      x1 <- int_x0 + CellLength/10
      y1 <- int_y0 + CellLength - CellLength/10
      
      
    }else if(Dir ==3){
      
      x0 <- int_x0 - CellLength/10
      y0 <- int_y0 + CellLength - CellLength/10
      
      x1 <- int_x0 -CellLength/10
      y1 <- int_y0 + CellLength/10
      
    }
  }else   if(Loc == 2){
    if(Dir == 2){
      
      x0 <- int_x0 - CellLength/10
      y0 <- int_y0 + CellLength/10
      
      x1 <- int_x0 - CellLength + CellLength/10
      y1 <- int_y0 + CellLength/10
      
    }else if(Dir ==4){
      
      x0 <- int_x0 - CellLength + CellLength/10
      y0 <- int_y0 -CellLength/10
      
      x1 <- int_x0 - CellLength/10
      y1 <- int_y0 - CellLength/10
      
    }
  }else   if(Loc == 3){
    if(Dir == 1){
      x0 <- int_x0 + CellLength/10
      y0 <- int_y0 - CellLength + CellLength/10
      
      x1 <- int_x0 + CellLength/10
      y1 <- int_y0 - CellLength/10
    }else if(Dir ==3){
      x0 <- int_x0 - CellLength/10
      y0 <- int_y0 - CellLength/10
      
      x1 <- int_x0 - CellLength/10
      y1 <- int_y0 - CellLength  + CellLength/10
    }
  }else   if(Loc == 4){
    if(Dir == 2){
      x0 <- int_x0 + CellLength  - CellLength/10
      y0 <- int_y0 + CellLength/10
      
      x1 <- int_x0 + CellLength/10
      y1 <- int_y0 + CellLength/10
    }else if(Dir ==4){
      x0 <- int_x0 + CellLength/10
      y0 <- int_y0 - CellLength/10
      
      x1 <- int_x0 + CellLength - CellLength/10
      y1 <- int_y0 - CellLength/10
    }
  }
  segment0$x0[m] <- x0
  segment0$y0[m] <- y0
  segment0$x1[m] <- x1
  segment0$y1[m] <- y1
  
}


# 
# segment1  <- Cell[which(Cell$Int == 0 & Cell$NetIn == 1),]
# 
# if(length(segment1$ID) > 0){
#   for(m in 1:length(segment1$ID)){
#     s_out <- segment1$S_out[m]
#     dir   <- segment1$Dir[m]
#     
#     x1 <- segment0$x0[which(segment0$ID == s_out)]
#     y1 <- segment0$y0[which(segment0$ID == s_out)]
#     
#     
#     if(dir == 1){
#     
#       x0 <- x1 
#       y0 <- y1 - CellLength
#       
#       y1 <- y1 - CellLength/5
#     }else if(dir == 2){
#       
#       x0 <- x1 + CellLength
#       y0 <- y1
#       
#       x1 <- x1 + CellLength/5
#       
#     }else if(dir == 3){
#       
#       x0 <- x1 
#       y0 <- y1 + CellLength
#       
#       
#       y1 <- y1 + CellLength/5
#     }else if(dir == 4){
#       
#       x0 <- x1 - CellLength
#       y0 <- y1
#       
#       x1 <- x1 - CellLength/5
#       
#     }
#     
#     segment1$x0[m] <- x0
#     segment1$y0[m] <- y0
#     segment1$x1[m] <- x1
#     segment1$y1[m] <- y1
#   }
# }
# 
# 
# 
# 
# 
# segment2  <- Cell[which(Cell$Int == 0 & Cell$NetOut == 1),]
# 
# if(length(segment2$ID > 0)){
#   for(m in 1:length(segment2$ID)){
#     s_in  <- segment2$S_in[m]
#     dir   <- segment2$Dir[m]
#     
#     x0 <- segment0$x1[which(segment0$ID == s_in)]
#     y0 <- segment0$y1[which(segment0$ID == s_in)]
#     
#     
#     if(dir == 1){
#       
#       x1 <- x0 
#       y1 <- y0 + CellLength
#       
#       y0 <- y0 + CellLength/5
#     }else if(dir == 2){
#       
#       x1 <- x0 - CellLength
#       y1 <- y0
#       
#       x0 <- x0 - CellLength/5
#       
#     }else if(dir == 3){
#       
#       x1 <- x0 
#       y1 <- y0 - CellLength
#       
#       
#       y0 <- y0 - CellLength/5
#       
#     }else if(dir == 4){
#       
#       x1 <- x0 + CellLength
#       y1 <- y0
#       
#       x0 <- x0 + CellLength/5
#       
#     }
#     
#     segment2$x0[m] <- x0
#     segment2$y0[m] <- y0
#     segment2$x1[m] <- x1
#     segment2$y1[m] <- y1
#   }
#   
# }
# 


# segment <- rbind( segment0 , segment1 , segment2 )
segment <- rbind( segment0  )

segment3  <- Cell[which(Cell$Int == 0 ),]
tempSegID <- segment3$ID


if(length(segment3$ID > 0)){
  while(length(tempSegID) > 0){
    temp <- c()
    for(m in 1:length(tempSegID)){
      
      m1 <- which(segment3$ID == tempSegID[m])
      
      s_in  <- segment3$S_in[m1]
      s_out <- segment3$S_out[m1]
      
      dir <- segment3$Dir[m1]
      
      if(s_in %in% segment$ID){
        # tempSegID <- tempSegID[-which(tempSegID == segment3$ID[m1])]
        temp <- append(temp, m1)
        
        x0 <- segment$x1[which(segment$ID == s_in  )]
        y0 <- segment$y1[which(segment$ID == s_in  )]
        
        if(dir == 1){
          
          x1 <- x0
          y1 <- y0 + CellLength
          
          y0 <- y0 + CellLength/5
          
          
        }else if(dir == 2){
          
          x1 <- x0 - CellLength
          y1 <- y0
          
          x0 <- x0 - CellLength/5
          
          
        }else if(dir == 3){
          
          x1 <- x0 
          y1 <- y0 - CellLength
          
          y0 <- y0 - CellLength/5
          
        }else if(dir == 4){
          
          x1 <- x0 + CellLength
          y1 <- y0
          
          x0 <- x0 + CellLength/5
          
        }
        
      }else if(s_out %in% segment$ID){
        
        temp <- append(temp, m1)
        x1 <- segment$x0[which(segment$ID == s_out  )]
        y1 <- segment$y0[which(segment$ID == s_out  )]
        
        if(dir == 1){
          
          x0 <- x1
          y0 <- y1 - CellLength
          
          y1 <- y1 - CellLength/5
          
        }else if(dir == 2){
          
          x0 <- x1 + CellLength
          y0 <- y1
          
          x1 <- x1 + CellLength/5
          
        }else if(dir == 3){
          
          x0 <- x1 
          y0 <- y1 + CellLength
          
          y1 <- y1 + CellLength/5
          
        }else if(dir == 4){
          
          x0 <- x1 - CellLength
          y0 <- y1
          
          x1 <- x1 - CellLength/5
          
        }
        
        
      }else{
        x0 <- 0
        y0 <- 0
        x1 <- 0
        y1 <- 0
      }
      
      
      segment3$x0[m1] <- x0
      segment3$y0[m1] <- y0
      segment3$x1[m1] <- x1
      segment3$y1[m1] <- y1
      
      
    }
    
    tempSegID <- segment3$ID[which(segment3$x0 == 0)]
    segment <- rbind(segment , segment3[temp,])
    
    
  }
  
  
  
  
}
# 
# dir.create(paste0(savepath,"Plot"),showWarnings = F)
# 
# network_shape <- ggplot() + geom_point(data = Intersection, aes(x = x , y =y)) + 
#   geom_segment(data = segment , aes( x=x0,y=y0,xend=x1,yend=y1) , arrow = arrow(length=unit(0.2, "cm"))  ) + geom_text(data= segment , aes(x=(x1+x0)/2,y=(y1+y0)/2,label=ID,angle= 90 * (Dir %in% c(1,3)), color  = "red")) + 
#   scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+ 
#   scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height)) 
# 
# ggsave(paste0(savepath,"Plot/network_shape.png"),network_shape , width= width/CellLength , height= height/CellLength)








colors <- RColorBrewer::brewer.pal(4,"Set1")

# + guides(color=FALSE) 
# 
# for(time0 in 1:timestep){
# 
#   tempsegment <- segment
# 
#   for(i in 1:length(tempsegment$ID)){
#     cellid0 <- tempsegment$ID[i]
# 
#     tempsegment$nresult[i] <- n[cellid0,time0]
#     tempsegment$vresult[i] <- v[cellid0,time0]
#   }
# 
#   tempIntersection <- Intersection
# 
#   tempIntersection$signal <- factor(1)
#   levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")
# 
#   for(i in 1:length(tempIntersection$ID)){
# 
#     sig <- IntSignal[as.numeric(Intersection$ID[i]),time0]
# 
#     if(sig == 1){
#       tempIntersection$signal[i] <-   "EW_S"
#     }else     if(sig == 2){
#       tempIntersection$signal[i] <-   "EW_L"
#     }else     if(sig == 3){
#       tempIntersection$signal[i] <-   "NS_S"
#     }else     if(sig == 4){
#       tempIntersection$signal[i] <-   "NS_L"
#     }
# 
#   }
#   tempIntersection$signal <- as.factor(tempIntersection$signal)
# 
#   # levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")
#   plot0 <-
#   ggplot() + geom_point(data = tempIntersection, aes(x = x , y =y , fill = signal   ) ,  shape = 21 , colour = "black" , size = 5 ) + scale_fill_discrete( name = "Signal" , drop = F ) +
#     geom_segment(data = tempsegment, aes( x=x0,y=y0,xend=x1,yend=y1,size = nresult , colour = vresult) , arrow = arrow(length=unit(0.2, "cm"))  ) +
#     scale_size_continuous(name = "line",breaks = c(seq(0, ceiling(CellLength * kjam /1000) , 2 ) , Inf) , range = c(0.1,3) , guide = guide_legend(title = "Number of Veh" ) , limits = c(0,max(as.vector(n))*1.5 )) +
#     scale_colour_gradient2(low = "red",mid = "yellow",high = "green", midpoint = vf/2 ,limits = c(0,vf) , "Speed")+
#     geom_text(data= tempsegment , aes(x=(x1+x0)/2,
#                                       y=(y1+y0)/2,
#                                       label=round(nresult,2) ,
#                                       angle= 90 * (Dir %in% c(1,3)) ) , color = "yellow" ,size = 3 ,fontface ="bold",check_overlap = T ) +
#     geom_text(data= tempsegment , aes(x=(x1+x0)/2,
#                                       y=(y1+y0)/2,
#                                       label=round(nresult,2) ,
#                                       angle= 90 * (Dir %in% c(1,3)) ) , color = "red" ,size = 3 ,check_overlap = T ) +
#     scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+
#     scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height))  + labs(title=paste0("time = ",time0))
# 
#   # print(plot0)
#   ggsave(paste0(savepath,"Plot/time_",time0,".png"),plot0 , width= width/CellLength + 1 , height= height/CellLength)
# 
# }

# 
# 
# 
# 

dir.create(paste0(savepath,"Plot_Agent"),showWarnings = F)

network_shape <- ggplot() + geom_point(data = Intersection, aes(x = x , y =y)) +
  geom_segment(data = segment , aes( x=x0,y=y0,xend=x1,yend=y1) , arrow = arrow(length=unit(0.2, "cm"))  ) + geom_text(data= segment , aes(x=(x1+x0)/2,y=(y1+y0)/2,label=ID,angle= 90 * (Dir %in% c(1,3)), color  = "red")) +
  scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+
  scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height))

ggsave(paste0(savepath,"Plot_Agent/network_shape.png"),network_shape , width= width/CellLength , height= height/CellLength)




n_agent = as.matrix(read.csv("n.csv",header =F))

v = as.matrix(read.csv("v.csv",header =F))



cl <- makeCluster(2)
registerDoParallel(cl, cores = 2)
getDoParName()


time111 <- proc.time()
AllFig <- list()

# system.time(
  #for(i in 1:100) {
  AllFig <- foreach(time0 = 1:timestep, .packages = c("ggplot2")) %dopar% {
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
      geom_segment(data = tempsegment , aes( x=x0,y=y0,xend=x1,yend=y1,size = nresult , colour = vresult) , arrow = arrow(length=unit(0.2, "cm"))  ) + 
      scale_size_continuous(name = "line",breaks = c(seq(0, ceiling(CellLength * kjam /1000) , 2 ) , Inf)  , range = c(0.1,3) , guide = guide_legend(title = "Number of Veh" ), limits = c(0,max(as.vector(n))*1.5 )) + 
      scale_colour_gradient2(low = "red",mid = "yellow",high = "green", midpoint = vf/2 ,limits = c(0,vf) , "Speed")+
      # geom_text(data= tempsegment , aes(x=(x1+x0)/2,
      #                                   y=(y1+y0)/2,
      #                                   label=round(nresult,2) ,
      #                                   angle= 90 * (Dir %in% c(1,3)) ) , color = "yellow" ,size = 1.5 ,fontface ="bold",check_overlap = T ) +
      geom_text(data= tempsegment , aes(x=(x1+x0)/2,
                                        y=(y1+y0)/2,
                                        label=round(nresult,2) ,
                                        angle= 90 * (Dir %in% c(1,3)) ) , color = "black" ,size = 3 ,check_overlap = T ,fontface = "bold") +
      scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+ 
      scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height))  + labs(title=paste0("time = ",time0))
    # ggsave(paste0(savepath , "Plot_agent/time_",time0,"png") , widht = 12 , height = 10)
    plot0
  }
# )

stopCluster(cl)

for(time0 in 1:length(AllFig)) {
  png(paste0(savepath,"Plot_agent/time_",time0,".png"), width = 1200, height = 1000)
  print(AllFig[[time0]])
  dev.off()
}



print(proc.time() - time111)



# 
# 
# for(time0 in 1:timestep){
# # foreach(time0 = 1:timestep) %dopar% {
#   print(time0)
#   tempsegment <- segment
# 
#   for(i in 1:length(tempsegment$ID)){
#     cellid0 <- tempsegment$ID[i]
# 
#     tempsegment$nresult[i] <- n_agent[cellid0,time0]
#     tempsegment$vresult[i] <- v[cellid0,time0]
#   }
# 
#   tempIntersection <- Intersection
# 
#   tempIntersection$signal <- factor(1)
#   levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")
# 
#   for(i in 1:length(tempIntersection$ID)){
# 
#     sig <- IntSignal[as.numeric(Intersection$ID[i]),time0]
# 
#     if(sig == 1){
#       tempIntersection$signal[i] <-   "EW_S"
#     }else     if(sig == 2){
#       tempIntersection$signal[i] <-   "EW_L"
#     }else     if(sig == 3){
#       tempIntersection$signal[i] <-   "NS_S"
#     }else     if(sig == 4){
#       tempIntersection$signal[i] <-   "NS_L"
#     }
# 
#   }
#   tempIntersection$signal <- as.factor(tempIntersection$signal)
# 
#   # levels(tempIntersection$signal)   <- c("EW_S","EW_L","NS_S","NS_L")
#   plot0 <-
#     ggplot() + geom_point(data = tempIntersection, aes(x = x , y =y , fill = signal   ) ,  shape = 21 , colour = "black" , size = 1.5 ) + scale_fill_discrete( name = "Signal" , drop = F ) +
#     geom_segment(data = tempsegment , aes( x=x0,y=y0,xend=x1,yend=y1,size = nresult , colour = vresult) , arrow = arrow(length=unit(0.2, "cm"))  ) +
#     scale_size_continuous(name = "line",breaks = c(seq(0, ceiling(CellLength * kjam /1000) , 2 ) , Inf)  , range = c(0.1,3) , guide = guide_legend(title = "Number of Veh" ), limits = c(0,max(as.vector(n))*1.5 )) +
#     scale_colour_gradient2(low = "red",mid = "yellow",high = "green", midpoint = vf/2 ,limits = c(0,vf) , "Speed")+
#     # geom_text(data= tempsegment , aes(x=(x1+x0)/2,
#     #                                   y=(y1+y0)/2,
#     #                                   label=round(nresult,2) ,
#     #                                   angle= 90 * (Dir %in% c(1,3)) ) , color = "yellow" ,size = 1.5 ,fontface ="bold",check_overlap = T ) +
#     geom_text(data= tempsegment , aes(x=(x1+x0)/2,
#                                       y=(y1+y0)/2,
#                                       label=round(nresult,2) ,
#                                       angle= 90 * (Dir %in% c(1,3)) ) , color = "black" ,size = 1.5 ,check_overlap = T ,fontface = "bold") +
#     scale_x_continuous(breaks = seq(0,width,CellLength) , label = seq(0,width,CellLength),limits = c(0,width))+
#     scale_y_continuous(breaks = seq(0,height,CellLength) , label = seq(0,height,CellLength),limits = c(0,height))  + labs(title=paste0("time = ",time0))
# 
#   # print(plot0)
#   # system.time(
#     ggsave(paste0(savepath,"Plot_agent/time_",time0,".png"),plot0 , width= (width/CellLength + 1)/3 + 1 , height= (height/CellLength)/3 , dpi = 150  )
#   # )
# 
# # system.time(
# time11 <- proc.time()
# png(filename = paste0(savepath,"Plot_agent/time_",time0,".png") , width =  1000 , height = 500)
# plot0
# dev.off()
# print(proc.time() - time11)
# 
# 
# # )
  
# }

