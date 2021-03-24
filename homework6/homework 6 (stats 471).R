

library("readxl")
y <- read_excel("yvalues.xlsx",col_names=FALSE)
y<-as.vector(y$...1)

f_y_x<-function(x,y,s){
  (1/sqrt(2*pi*s))*exp(-(x-y)^2/(2*s))
}

A<-function(y,sigma_k){
  a=((1-y)^2*f_y_x(1,y,sigma_k))/(f_y_x(1,y,sigma_k)+f_y_x(0,y,sigma_k))
  b=((0-y)^2*f_y_x(0,y,sigma_k))/(f_y_x(1,y,sigma_k)+f_y_x(0,y,sigma_k))
  sum(a+b)
}

a<-function(q,x_i,x_k){
  q*ifelse(x_i==x_k,1,0)+(1-q)*ifelse(x_i!=x_k,1,0)
}
sigma_2<-1
max_q<-0.5
for (i in 1:1000) {
  sigma_2<-sum(A(y,sigma_2))/1000
  upper<-a(max_q,1,1)*f_y_x(1,y,sigma_2)[1:999]*f_y_x(1,y,sigma_2)[2:1000]+
    a(max_q,0,0)*f_y_x(0,y,sigma_2)[1:999]*f_y_x(0,y,sigma_2)[2:1000]
  down<-a(max_q,1,1)*f_y_x(1,y,sigma_2)[1:999]*f_y_x(1,y,sigma_2)[2:1000]+
    a(max_q,0,0)*f_y_x(0,y,sigma_2)[1:999]*f_y_x(0,y,sigma_2)[2:1000] +
    a(max_q,0,1)*f_y_x(0,y,sigma_2)[1:999]*f_y_x(1,y,sigma_2)[2:1000]+
    a(max_q,1,0)*f_y_x(1,y,sigma_2)[1:999]*f_y_x(0,y,sigma_2)[2:1000]
  p<-upper/down
  max_q<-sum(p)/999
}
max_q
sigma_2




B<-function(y_1,y_2,sigma_2,max_q){
  #y given x
  y_given_x1<-f_y_x(1,y_1,sigma_2)/(f_y_x(1,y_1,sigma_2)+f_y_x(0,y_1,sigma_2))
  y_given_x0<-f_y_x(0,y_1,sigma_2)/(f_y_x(1,y_1,sigma_2)+f_y_x(0,y_1,sigma_2))
  
  #p(x_i+1  | y_i+1*, y_i*, x_i = 0), denote cases by X_i+1 = x0_i1_0 means X_i+1 was set to 0
  x0_i1_1<-a(max_q,1,0)*f_y_x(1,y_2,sigma_2)
  x0_i1_0<-a(max_q,0,0)*f_y_x(0,y_2,sigma_2)
  p10<-x0_i1_1/(x0_i1_1+x0_i1_0)
  
  #p(x_i+1  | y_i+1*, y_i*, x_i = 1), denote cases by X_i+1 = x1_i1_1 means X_i+1 was set to 0
  x1_i1_1<-a(max_q,1,1)*f_y_x(1,y_2,sigma_2)
  x1_i1_0<-a(max_q,1,0)*f_y_x(0,y_2,sigma_2)
  p11<-x1_i1_1/(x1_i1_1+x1_i1_0)
  y_given_x1*p11+y_given_x0*p10
  
}


bs<-c()
for (i in 1:999) {
  b<-B(y[i],y[i+1],sigma_2,max_q)
  bs<-c(bs,b)
}

sum(bs)





for (i in 1:20) {
  bs<-c()
  for (i in 1:999) {
    b<-B(y[i],y[i+1],sigma_2,max_q)
    bs<-c(bs,b)
  }
  sigma_2<-A(y,sigma_2)/1000
  max_q<-sum(bs)/999
  print(max_q)
}

