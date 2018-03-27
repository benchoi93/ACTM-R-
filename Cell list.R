
Cell_list <- list()
for(l0 in 1:length(Cell$ID)){

  Cell_list[[l0]] <- list(ID = Cell$ID[l0] , length = Cell$Length[l0] , Straight = c(0) , Left = c(0) , Right = c(0), Out = c(0)  , Wait = c(0)) 

}

Veh_list <- list()

Veh_data <- data.frame(matrix(0, nrow = 1 , ncol  = 5))
colnames(Veh_data) <- c("ID","CurCell","NextCell","Direction","time")





update_cell <-c()
origin_set <- Cell$ID
for(i in 1:length(origin_set)){
  O_cell <- origin_set[i]
  
  S_cell <- Cell$S_out[which(Cell$ID == O_cell)]
  R_cell <- Cell$R_out[which(Cell$ID == O_cell)]
  L_cell <- Cell$L_out[which(Cell$ID == O_cell)]
  
  
  if(S_cell != 0){
    update_cell <- rbind(update_cell , c(O_cell , S_cell , 1,0))
  }
  
  if(R_cell != 0){
    update_cell <- rbind(update_cell , c(O_cell , R_cell, 3,0))
  }
  
  if(L_cell != 0){
    update_cell <- rbind(update_cell , c(O_cell , L_cell, 2,0))
  }
  
}