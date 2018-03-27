
# 
# for(i in 1:length(DemandCell$ID)){
#   cellid <- DemandCell$ID[i]
#   int0 <- Cell$Int[which(Cell$ID == cellid)]
#   while(int0 == 0){
#     cellid <- Cell$S_out[which(Cell$ID == cellid)]
#     int0 <- Cell$Int[which(Cell$ID == cellid)]
#   }
#   
#   
#   DemandCell$Node_Int[i] <- int0
# }
# 
# 
# 
# for(i in 1:length(SinkCell$ID)){
#   cellid <- SinkCell$ID[i]
#   int0 <- Cell$Int[which(Cell$ID == cellid)]
#   while(int0 == 0){
#     cellid <- Cell$S_in[which(Cell$ID == cellid)]
#     int0 <- Cell$Int[which(Cell$ID == cellid)]
#   }
#   
#   
#   SinkCell$Node_Int[i] <- int0
# }

edges <- c()
for(i in 1:length(Cell$ID)){
  
  
  cellid <- Cell$ID[i]
  sout   <- Cell$S_out[i]
  lout   <- Cell$L_out[i]
  rout   <- Cell$R_out[i]
  
  edges <- rbind(edges , c(cellid , sout),c(cellid , lout),c(cellid , rout) )
}

edges <- edges[which(edges[,2] != 0) , ]



graph <- make_directed_graph(n = length(Cell$ID) , edges = t(edges))
# graph <- add_vertices(graph = graph , nv = length(Cell$ID)/2 )


ODpair <- c()
for(i in 1:length(DemandCell$ID)){
  for(j in 1:length(SinkCell$ID)){
    # if(i != j){
      path <- shortest_paths(graph , from = DemandCell$ID[i] , to = SinkCell$ID[j])
      path <- path$vpath[[1]]
      
      if(length(path) != 0){
        ODpair <- rbind(ODpair , c(DemandCell$ID[i] , SinkCell$ID[j]))
      }
      
    # }
  }
}






# 
# 
# graph <- make_lattice(dimvector = c(NetRow,NetCol))
# 
# for(i in 1:length(DemandCell$ID)){
#   
#   int0 <- DemandCell$Node_Int[i]
#   
#   graph <- add_vertices(graph = graph , nv = 1)
#   graph <- add_edges(graph = graph , edges =  c(int0 , NetRow*NetCol + i))
#   
#   DemandCell$Node[i] <- NetRow*NetCol + i
# }
# 
# 
# 
# 
# 
# 
# 
# 
# for(i in 1:length(SinkCell$ID)){
#   
#   int0 <- SinkCell$Node_Int[i]
#   
#   SinkCell[i,]
#   
#   temp <- which(DemandCell$Node_Int == int0)
#   
#   
#   
#   for(k in 1:length(temp)){
#     
#     dir_sink   <- SinkCell$Dir[i]
#     dir_demand <- DemandCell$Dir[temp[k]]
#     
#     
#     if(dir_sink == 1 & dir_demand == 3){
#       SinkCell$Node[i] <- DemandCell$Node[temp[k]]
#     }else if(dir_sink == 2 & dir_demand == 4){
#       SinkCell$Node[i] <- DemandCell$Node[temp[k]]
#     }else if(dir_sink == 3 & dir_demand == 1){
#       SinkCell$Node[i] <- DemandCell$Node[temp[k]]
#     }else if(dir_sink == 4 & dir_demand == 2){
#       SinkCell$Node[i] <- DemandCell$Node[temp[k]]
#     }
#     
#     
#   }
#   
#   
#   
#   
# }



get_Node_ID <- function(CellID){
  return(DemandCell$Node[which(DemandCell$ID == CellID)])
}


