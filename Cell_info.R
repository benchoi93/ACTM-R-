Cell_info <- function(m,n,a,b){
  
  ## m: number of row, n: number of col, a: section/link, b: section in end
  
  north <- matrix(0   ,  n*(a*(m-1)+2*b)    ,13)
  east <- matrix(0,      m*(a*(n-1)+2*b)    ,13)
  south<-matrix(0,       n*(a*(m-1)+2*b)    ,13)
  west<-matrix(0,m*(a*(n-1)+2*b),13)
  
  colnames(north) <- c("ID" ,"Length", "NetIn","NetOut","Int","Loc","Dir" ,"S_in","L_in","R_in","S_out","L_out","R_out")
  colnames(east)  <- c("ID" ,"Length", "NetIn","NetOut","Int","Loc","Dir" ,"S_in","L_in","R_in","S_out","L_out","R_out")
  colnames(south) <- c("ID" ,"Length", "NetIn","NetOut","Int","Loc","Dir" ,"S_in","L_in","R_in","S_out","L_out","R_out")
  colnames(west)  <- c("ID" ,"Length", "NetIn","NetOut","Int","Loc","Dir" ,"S_in","L_in","R_in","S_out","L_out","R_out")
  
  ## ID & Length
  
  north[,1]   <-  1:nrow(north)
  east[,1]    <-  (nrow(north)+1) : (nrow(north)+nrow(east))
  south[,1]   <-  (nrow(north)+nrow(east)+1) : (nrow(north)+nrow(east)+nrow(south))
  west[,1]    <-  (nrow(north)+nrow(east)+nrow(south)+1) : (nrow(north)+nrow(east)+nrow(south)+nrow(west))
  
  north[,2] <-100
  east[,2] <- 100
  south[,2] <- 100
  west[,2] <- 100
  
  ## Network in & out
  for (i in 1:n) {
    for (j in 1: nrow(north)){
      if (north[j,1]==(i-1)*(nrow(north)/n)+1){
        north[j,3]<-1}
      if (north[j,1]==i*(nrow(north)/n))
        north[j,4]<-1
        }}
  
  for (i in 1:m) {
    for (j in 1: nrow(east)){
      if (east[j,1]==nrow(north)+(i-1)*(nrow(east)/m)+1){
        east[j,3]<-1}
      if (east[j,1]==nrow(north)+i*(nrow(east)/m))
        east[j,4]<-1    
      }}
  
  for (i in 1:n) {
    for (j in 1: nrow(south)){
      if (south[j,1]==nrow(north)+nrow(east)+(i-1)*(nrow(south)/n)+1){
        south[j,3]<-1}
      if (south[j,1]==nrow(north)+nrow(east)+i*(nrow(south)/n))
        south[j,4]<-1
    }}
  
  for (i in 1:m) {
    for (j in 1: nrow(west)){
      if (west[j,1]==nrow(north)+nrow(east)+nrow(south)+(i-1)*(nrow(west)/m)+1){
        west[j,3]<-1}
      if (west[j,1]==nrow(north)+nrow(east)+nrow(south)+i*(nrow(west)/m))
        west[j,4]<-1
    }}
  
  for (i in 1:(m-1)) {
    for (j in 1:(n-1)) {
      for (k in 1:nrow(north)) {
        if ((north[k,1]>(j-1)*(nrow(north)/n)+b+a*(i-1)+1) && (north[k,1]<(j-1)*(nrow(north)/n)+b+a*i)) {
          north[k,3]<- 1
          north[k,4]<- 1  
        }}}}
  
  for (i in 1:(m-1)) {
    for (j in 1:(n-1)) {
      for (k in 1:nrow(east)) {
        if ((east[k,1]>nrow(north)+(i-1)*(nrow(east)/m)+b+a*(j-1)+1) && (east[k,1]<nrow(north)+(i-1)*(nrow(east)/m)+b+a*j)) {
          east[k,3]<- 1
          east[k,4]<- 1  
        }}}}
  
  for (i in 1:(m-1)) {
    for (j in 1:(n-1)) {
      for (k in 1:nrow(south)) {
        if ((south[k,1]>nrow(north)+nrow(east)+(j-1)*(nrow(south)/n)+b+a*(i-1)+1) && (south[k,1]<nrow(north)+nrow(east)+(j-1)*(nrow(south)/n)+b+a*i)) {
          south[k,3]<- 1
          south[k,4]<- 1  
        }}}}
  
  for (i in 1:(m-1)) {
    for (j in 1:(n-1)) {
      for (k in 1:nrow(west)) {
        if ((west[k,1]>nrow(north)+nrow(east)+nrow(south)+(i-1)*(nrow(west)/m)+b+a*(j-1)+1) && (west[k,1]<nrow(north)+nrow(east)+nrow(south)+(i-1)*(nrow(west)/m)+b+a*j)) {
          west[k,3]<- 1
          west[k,4]<- 1 
          }}}}
  
  
  ## Int Loc Dir
  
  for (i in 1:m){
    for (j in 1:n){
      for (k in 1: nrow(north)){
        north[k,7]<-3
        if (north[k,1]==(j-1)*(nrow(north)/n)+(i-1)*a+b){
          north[k,5]<-m*(j-1)+i
          north[k,6]<-1}
        if (north[k,1]==(j-1)*(nrow(north)/n)+(i-1)*a+b+1){
          north[k,5]<-m*(j-1)+i
          north[k,6]<-3}
        }}}
  
  for (i in 1:m){
    for (j in 1:n){
      for (k in 1:nrow(east)){
        east[k,7] <- 2
        if (east[k,1]==nrow(north)+(i-1)*(nrow(east)/m)+(j-1)*a+b){
          east[k,5] <- (n-1)*m+1-(j-1)*m+(i-1)
          east[k,6] <- 4}
        if (east[k,1]==nrow(north)+(i-1)*(nrow(east)/m)+(j-1)*a+b+1){
          east[k,5] <- (n-1)*m+1-(j-1)*m+(i-1)
          east[k,6] <- 2} 
      }}}
    
  for (i in 1:m){
    for (j in 1:n){
      for (k in 1: nrow(south)){
        south[k,7]<-1
        if (south[k,1]==(nrow(north)+nrow(east)+(j-1)*(nrow(south)/n)+(i-1)*a+b)){
          south[k,5]<-m*n-(i-1)-m*(j-1)
          south[k,6]<-3}
        if (south[k,1]==(nrow(north)+nrow(east)+(j-1)*(nrow(south)/n)+(i-1)*a+b+1)){
          south[k,5]<-m*n-(i-1)-m*(j-1)
          south[k,6]<-1}
      }}}
  
  for (i in 1:m){
    for (j in 1:n){
      for (k in 1:nrow(west)){
        west[k,7] <- 4
        if (west[k,1]==nrow(north)+nrow(east)+nrow(south)+(i-1)*(nrow(west)/m)+(j-1)*a+b){
          west[k,5] <- m+m*(j-1)-(i-1)
          west[k,6] <- 2}
        if (west[k,1]==nrow(north)+nrow(east)+nrow(south)+(i-1)*(nrow(west)/m)+(j-1)*a+b+1){
          west[k,5] <- m+m*(j-1)-(i-1)
          west[k,6] <- 4} 
      }}}
  
  ## S_in & S_out
  for (i in 1:nrow(north)){
    north[i,8]<-north[i,1]-1
    north[i,11]<-north[i,1]+1}
  for (i in 1:n){
    north[((i-1)*(nrow(north)/n))+1,8]<-0
    north[(i*(nrow(north)/n)),11]<-0
  }
  
  for (i in 1:nrow(east)){
    east[i,8]<-east[i,1]-1
    east[i,11]<-east[i,1]+1}
  for (i in 1:m){
    east[((i-1)*(nrow(east)/m))+1,8]<-0
    east[(i*(nrow(east)/m)),11]<-0
  }
  
  for (i in 1:nrow(south)){
    south[i,8]<-south[i,1]-1
    south[i,11]<-south[i,1]+1}
  for (i in 1:n){
    south[((i-1)*(nrow(south)/n))+1,8]<-0
    south[(i*(nrow(south)/n)),11]<-0
  }
  
  for (i in 1:nrow(west)){
    west[i,8]<-west[i,1]-1
    west[i,11]<-west[i,1]+1}
  for (i in 1:m){
    west[((i-1)*(nrow(west)/m))+1,8]<-0
    west[(i*(nrow(west)/m)),11]<-0
  }
  
  ## L in L out
  for (k in 1:(m*n)){
    for (i in 1:nrow(north)){
      for (j in 1:nrow(east)){
        if ((north[i,5]==k) & (north[i,6]==3) & (east[j,5]==k) & (east[j,6]==4)){
          north[i,9] <- east[j,1]
          east[j,12] <- north[i,1]
        }}}}
  
  for (i in 1:nrow(east)){
    for (j in 1:nrow(south)){
      for (k in 1:(m*n)){
        if (east[i,5]==k & east[i,6]==2 & south[j,5]==k & south[j,6]==3){
          east[i,9]<-south[j,1]
          south[j,12]<-east[i,1]
        }}}}
  
  for (i in 1:nrow(south)){
    for (j in 1:nrow(west)){
      for (k in 1:(m*n)){
        if (south[i,5]==k & south[i,6]==1 & west[j,5]==k & west[j,6]==2){
          south[i,9]<-west[j,1]
          west[j,12]<-south[i,1]
        }}}}
  
  for (i in 1:nrow(west)){
    for (j in 1:nrow(north)){
      for (k in 1:(m*n)){
        if (west[i,5]==k & west[i,6]==4 & north[j,5]==k & north[j,6]==1){
          west[i,9]<-north[j,1]
          north[j,12]<-west[i,1]
        }}}}
  
  ## R in R out
  for (k in 1:(m*n)){
    for (i in 1:nrow(north)){
      for (j in 1:nrow(west)){
        if ((north[i,5]==k) & (north[i,6]==3) & (west[j,5]==k) & (west[j,6]==2)){
          north[i,10] <- west[j,1]
          west[j,13] <- north[i,1]
        }}}}
  
  for (k in 1:(m*n)){
    for (i in 1:nrow(west)){
      for (j in 1:nrow(south)){
        if ((west[i,5]==k) & (west[i,6]==4) & (south[j,5]==k) & (south[j,6]==3)){
          west[i,10] <- south[j,1]
          south[j,13] <- west[i,1]
        }}}}
  
  for (k in 1:(m*n)){
    for (i in 1:nrow(south)){
      for (j in 1:nrow(east)){
        if ((south[i,5]==k) & (south[i,6]==1) & (east[j,5]==k) & (east[j,6]==4)){
          south[i,10] <- east[j,1]
          east[j,13] <- south[i,1]
        }}}}
  
  for (k in 1:(m*n)){
    for (i in 1:nrow(east)){
      for (j in 1:nrow(north)){
        if ((east[i,5]==k) & (east[i,6]==2) & (north[j,5]==k) & (north[j,6]==1)){
          east[i,10] <- north[j,1]
          north[j,13] <- east[i,1]
        }}}}
  
  Cell<-rbind(north, east, south, west)
  return(Cell)
}