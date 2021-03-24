
library("readxl")
y <- read_excel("yvalues.xlsx",col_names=FALSE)

part3<-function(x,y){
  exp(-(x-y)^2/2)/2
}
a<-function(x_k1,x_k2){
  q=0.95
  return(q*ifelse(x_k2==x_k1,1,0)+(1-q)*ifelse(x_k2!=x_k1,1,0))
}

x_part1<-c()
for (i in y$...1) {
  if(part3(1,i)>part3(0,i)){
    x_part1<-c(x_part1,1)
  }
  else{
    x_part1<-c(x_part1,0)
  }
}
plot(1:1000,x_part1, lty=1, ylim = c(-2,2))
points(1:1000,y$...1, col="red", pch="*")
lines(1:1000,y$...1, col="red",lty=2)

##part 2

#construct look up table 
max_prob  <- matrix(NA, nrow=1000, ncol=2)
maxizer_n <- matrix(NA, nrow=1000, ncol=2)
row_count=1
for (y_i in y$...1) {
  max_p<-c()
  max_n<-c()
  for (x2 in 0:1) {
    zero<-ifelse(row_count<1000,log(a(0,x2)*part3(0,y_i))+ifelse(row_count==1,0,max_prob[row_count-1,1]), 
                 log(part3(0,y_i))+ifelse(row_count==1,0,max_prob[row_count-1,1]))
    one<-ifelse(row_count<1000,log(a(1,x2))+log(part3(1,y_i))+ifelse(row_count==1,0,max_prob[row_count-1,2]), 
                log(part3(0,y_i))+ifelse(row_count==1,0,max_prob[row_count-1,1]))
    max_p_i<-ifelse(zero>one,zero,one )
    max_n_i<-ifelse(zero>one,0,1 )
    max_p<-c(max_p,max_p_i)
    max_n<-c(max_n,max_n_i)
    
  }
  max_prob[row_count,]<-max_p
  maxizer_n[row_count,]<-max_n
  row_count=row_count+1
}

# go backward 
x_max=1
xs<-c(x_max)
for (i in 999:1) {
  x_max=maxizer_n[i,x_max+1]
  xs<-c(xs,x_max)
}
plot(1:1000,xs[1000:1], lty=1,ylim = c(-2,2))
points(1:1000,y$...1, col="red", pch="*")
lines(1:1000,y$...1, col="red",lty=2)


dp_ex<-exp(sum(xs)/10-25)
dp_ex








###########################
a<-function(x_k1,x_k2){
  q=0.95
  return(q*ifelse(x_k2==x_k1,1,0)+(1-q)*ifelse(x_k2!=x_k1,1,0))
}
part3<-function(x,y){
  exp(-(x-y)^2/2)/2
}
index_1<-function(x1,x2,y1){
  upper<-a(x1,x2)*part3(x1,y1)
  bottom<-a(1,x2)*part3(1,y1)+a(0,x2)*part3(0,y1)
  bias<-upper/bottom
  u<-runif(1)
  ifelse(bias>u,x1,ifelse(x1==1,0,1))
}
index_middle<-function(x_i,x1,x2,y_i){
  upper<-a(x1,x_i)*a(x2,x_i)*part3(x_i,y_i)
  bottom<-a(x1,0)*a(x2,0)*part3(0,y_i)+a(x1,1)*a(x2,1)*part3(1,y_i)
  bias<-upper/bottom
  u<-runif(1)
  ifelse(bias>u,x_i,ifelse(x_i==1,0,1))
}
index_n<-function(x_n,x_other,y_n){
  upper<-a(x_other,x_n)*part3(x_n,y_n)
  bottom<-a(x_other,1)*part3(1,y_n)+a(x_other,0)*part3(0,y_n)
  bias<-upper/bottom
  u<-runif(1)
  ifelse(bias>u,x_n,ifelse(x_n==1,0,1))
}

#x<-c(rep(1,500),rep(0,500))
#x<-c(rep(1,1000))
x<-c(rep(1,1000))
expects<-c()
n=100000
for (i in 1:n) {
  index<-sample(1:1000,1,prob = rep(1/1000,1000))
  x[index]<-ifelse(index==1,index_1(x[index],x[index+1],y$...1[index]),
                   ifelse(index==1000, index_n(x[index],x[index-1],y$...1[index]), 
                          index_middle(x[index],x[index-1],x[index+1],y$...1[index])))
  expect<-sum(x)
  expects<-c(expects,expect)
  

}

expects<-exp(expects/10-25)
iter_exp<-c()
for (i in 1:n) {
  a<-sum(expects[1:i])/i
  iter_exp<-c(iter_exp,a)
}

png("init_1.png", 
    height = 200, width = 200, units = "mm", res = 300)
print(plot(1:n,iter_exp,pch = ".",main = 'init 1'))
dev.off()


gibb_ex<-sum(exp(expects/10-25))/100000

## this is done through high throughput computing, it still took about ~4 minutes
## I tried to use local computer but it took too long and converging very slowly 

## First I tried n = 1000 on my local computer, it took seconds but there is barely any convergance.
## Then I tried n = 100000 in my local computer, 
## but it took a long time and my computer is getting to hot (my computer is old and )
## So I switched to high throughput computing, it took ~2 minutes and the result shows more convergence
## Last I tried n = 1100000 (the 100000 is carried over in the last iteration), it took ~4 minutes.
## While the result shows even more convergence, it is not as obvious as the transition from n = 1000 to n = 100000


################################ising[row,col]


finding_nbr<-function(index_row,index_col){
if(index_row==1 ){
  if(index_col ==1){c(ising[index_row,index_col+1],ising[index_row+1,index_col])}
  if(index_col ==50){c(ising[index_row,index_col-1],ising[index_row+1,index_col])}
  else{c(ising[index_row,index_col-1],ising[index_row+1,index_col],c(ising[index_row,index_col+1]))}
}

if(index_row==50 ){
  if(index_col ==1){c(ising[index_row,index_col+1],ising[index_row-1,index_col])}
  if(index_col ==50){c(ising[index_row,index_col-1],ising[index_row-1,index_col])}
  else{c(ising[index_row,index_col-1],ising[index_row-1,index_col],c(ising[index_row,index_col+1]))}
}
if(index_row!=50 & index_col!=50&index_row!=1 & index_col!=1){
  c(ising[index_row,index_col-1],
     ising[index_row-1,index_col],
   ising[index_row,index_col+1],
   ising[index_row+1,index_col])
}}

bias<-function(index_row,index_col,temperture){
  upper=exp(-(1/temperture)*ising[index_row,index_col]*-(sum(finding_nbr(index_row,index_col))))
  bottom = exp(-(1/temperture)*1*-(sum(finding_nbr(index_row,index_col)))) + exp(0)
  upper/bottom
}





#update
temperture<-10000 # or any other settings 
ising<- matrix(0, nrow=50, ncol=50)
for(i in 1:10000){
  u=runif(1)
  index_row<-sample(1:50,1,prob = rep(1/50,50))
  index_col<-sample(1:50,1,prob = rep(1/50,50))
  bia_i<-bias(index_row,index_col,temperture)
  ising[index_row,index_col]<-ifelse(bia_i>u,ising[index_row,index_col],ifelse(ising[index_row,index_col]==1,0,1))
  
}
  
image(ising,dendrogram='none', Rowv=FALSE, Colv=FALSE,trace='none')

