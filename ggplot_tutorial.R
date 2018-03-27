

# R + ggplot2 tutorial

a <- 1
b = 1
c -> 1
print(a)
print(b)
print(c)


a == b
a != b

c <- a+b
print(c)

typeof(a)
typeof(b)
typeof(c)

a = as.integer(a)
typeof(a)

################################
# type or storage mode of object
################################
# "logical", 
# "integer", 
# "double", 
# "complex", 
# "character",
# "raw" and "list", 
# "NULL", 
# "closure" (function), 
# "special" and "builtin"
# "environment",
################################

a = c(1,2,3)
print(a)

append(a, 4)
a

a <- append(a, 4)
a
################################

a = c(1,2,3)
b = c(4,5,6)

a1 = append(a,b)
print(a1)
length(a1)
dim(a1)

a2 = rbind(a,b)
print(a2)
length(a2)
dim(a2)

a3 = cbind(a,b)
print(a3)
length(a3)
dim(a3)

################################

a = seq(1,100,1)

a[a %% 2 == 0]
a[which(a %% 2 == 0)]
subset(a , a%%2 == 0)

b = a %% 2 == 0
c = a %% 3 == 0

b
c

b & c
b && c

# ‘&’ and ‘&&’ indicate logical AND and ‘|’ and ‘||’ indicate
# logical OR.  The shorter form performs elementwise comparisons in
# much the same way as arithmetic operators.  The longer form
# evaluates left to right examining only the first element of each
# vector.  Evaluation proceeds only until the result is determined.
# The longer form is appropriate for programming control-flow and
# typically preferred in ‘if’ clauses.


a[b & c]
a[b | c]


################################

a = c(1,2,3)
b = c("one","two","three")

df_a = data.frame(a)
df_b = data.frame(b  , stringsAsFactors = F)

df = cbind(df_a,df_b)
colnames(df) = c("number","name")
df

df$number
df[,1]
df[,"number"]

df$name
df[,2]
df[,"name"]

typeof(df) # list with same row length

################################

x_plus_3 = function(x){
  return(x+3)
}

df = matrix(1:6, nrow = 2)

df_initial = df
for(i in 1:dim(df)[1]){
  for(j in 1:dim(df)[2]){
    df[i,j] = x_plus_3(df[i,j])
  }
}
df

df <- df_initial
df
sapply(df , FUN = x_plus_3)
lapply(df , FUN = x_plus_3)

apply(df , MARGIN = c(1,2) , FUN =  x_plus_3)
apply(df , MARGIN = c(1) , FUN =  x_plus_3)
apply(df , MARGIN = c(2) , FUN =  x_plus_3)

################################

a = 1:100
a

for(i in 1:10){
  a[i] = x_plus_3(a[i])
}
a

a = 1:100
n = 1
while(n <= 10){
  a[n] <- x_plus_3(a[n]) 
  n <- n+1
}

for(i in 1:100){
  if(i != a[i]){
    print(a[i])
  }
}

################################


install.packages("ggplot2")
library(ggplot2)

a = 1:100
b = rnorm(100)

df = data.frame(cbind(a,b))

head(df)

# plotting without ggplot
plot(df$a , df$b)
###



ggplot(data = df , aes(x = a , y = b)) + geom_point()
ggplot(data = df , aes(x = a , y = b)) + geom_line()
ggplot(data = df , aes(x = a , y = b)) + geom_point() + geom_smooth()
ggplot(data = df , aes(x = a , y = b)) + geom_point() + geom_smooth(method = "lm")

ggplot(data = df , aes(x = b )) + geom_histogram(bins = 10)
ggplot(data = df , aes(x = b )) + geom_histogram(bins = 30)
ggplot(data = df , aes(x = b )) + geom_histogram(bins = 100)

ggplot(data = df , aes(x = b )) + stat_ecdf( )
ggplot(data = df , aes(x = b )) + stat_ecdf( geom = "step")
ggplot(data = df , aes(x = b )) + stat_ecdf( geom = "point")
ggplot(data = df , aes(x = b )) + stat_ecdf( geom = "line")

df$c = rep(1:10, each = 10)
ggplot(df , aes(x = a , y =b , color = c)) + geom_point()
ggplot(df , aes(x = a , y =b , color = factor(c))) + geom_point()



################################

data <- read_csv("~/Google Drive/KAIST/ANS_Simulator/data.txt",  col_names = FALSE)
data = as.data.frame(data)
head(data)
colnames(data) = c("Time","MP","Speed","Flow")

ggplot(data , aes( x= Time , y = Speed)) + geom_point()
ggplot(data , aes( x= Time , y = Speed , color = MP)) + geom_point()
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_point()
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_point() + guides(color = F)
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_line() + guides(color = F)
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_line() + guides(color = F)  + geom_abline(slope = 0 , intercept = 40 , color = "red")
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_line() + guides(color = F)  + geom_abline(slope = 0 , intercept = 40 , color = "red") + geom_abline(slope = 0 , intercept = 60 , color = "blue")


ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_line() + guides(color = F)

levels(factor(data$MP))
length(levels(factor(data$MP)))

RColorBrewer::display.brewer.all()
new_colors = c(RColorBrewer::brewer.pal(12 , "Set3"),RColorBrewer::brewer.pal(8 , "Set2"),RColorBrewer::brewer.pal(3 , "Set1")[1:2])
ggplot(data , aes( x= Time , y = Speed , color = factor(MP))) + geom_line()  + scale_color_manual(values = new_colors)





ggplot(data , aes( x= Time , y = Flow , color = factor(MP))) + geom_line() + guides(color = F)

ggplot(data , aes( x= Flow , y = Speed , color = factor(MP))) + geom_point() + guides(color = F)
ggplot(data , aes( x= Flow/Speed , y = Flow , color = factor(MP))) + geom_point() + guides(color = F)
ggplot(data , aes( x= Flow/Speed , y = Flow , color = factor(MP))) + geom_line() + guides(color = F)


ggplot(data , aes( x= (Time) , y = Speed )) + geom_point() + facet_wrap(~MP)
ggplot(data , aes( x= (Time) , y = Speed )) + geom_point() + facet_wrap(~MP , nrow = 3)
ggplot(data , aes( x= (Time) , y = Speed )) + geom_point() + facet_wrap(~MP , ncol = 5)

ggplot(data , aes( x= (Time) , y = Speed , color = Flow)) + geom_point() + facet_wrap(~MP , ncol = 5)
ggplot(data , aes( x= (Time) , y = Speed , color = Flow)) + geom_point() + facet_wrap(~MP , ncol = 5) + scale_color_gradient(low = "red", high = "blue")
ggplot(data , aes( x= (Time) , y = Speed , color = Flow)) + geom_point() + facet_wrap(~MP , ncol = 5) + scale_color_gradient2(low = "red" , mid = "yellow" , high ="green" , midpoint = 1000)









