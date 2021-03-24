library(dplyr)
###question 1
# A
# yes it is

# find inverse CDF and put uniform dist in it to generate new samples with pdf of f
unif<-runif(1000,0,1)
new_x<-unif^2
hist(new_x)

###question 2
g <- function(x) {
  ((sin(x))**2)*abs(x**3+2*x-3)
  
}

re_run<-0
f_x<-rep(0,1000)
x_select<-c()
y_select<-c()
M<-4.5

for (i in 1:length(f_x)) {
  repeat{
    
    x<-runif(1,-1,1)
    y<-runif(1,0,M)
    re_run<-re_run+1
    if(((x > -1 & x<0)| (x>0.5 &x<1)) & 0<=y & y<=g(x)){break}
  }
  x_select<-c(x_select,x)
  y_select<-c(y_select,y)
}

his<-hist(x_select,breaks = 30)
str(his)
his$counts
his$counts<-his$counts/(sum(his$counts)*0.05)
plot(his,ylim = c(0,4))
# 

range<-seq(-1,1,0.01)
lines(range,g(range),type = 'l')


## ----c-----
g_val<- 1000/re_run*2*4.5
c2<-1/g_val
c2
# c = 0.6196667


#question 3
p_3 <- function(x) {
  (x^-0.5)/2
  
}
g_3<-function(x){
  (x^-0.5)/2*abs((cos(x**2+x)))**0.5
}
run_3<-0
p_y<-rep(0,1000)
x_select_3<-c()
y_select_3<-c()
for (i in 1:length(p_y)) {
    repeat{
      run_3<-run_3+1
      in_unif<-runif(1,0,1)
      x<-in_unif^2
      y<-runif(1,0,p_3(x))
      re_run<-re_run+1
      if(0<=y & y<=g_3(x)){break}
    }
  x_select_3<-c(x_select_3,x)
  y_select_3<-c(y_select_3,y)
  }

plot(x_select_3,y_select_3)
hist(x_select_3)

his1<-hist(x_select_3,breaks = 30)
str(his1)
his1$counts
his1$counts<-his1$counts/(sum(his1$counts)*0.05)
plot(his1,ylim = c(0,4))

##calcuate c
c_3<- 1/(1000/run_3)
# c_3 = 1.157


range<-seq(0,1,0.01)
lines(range,c_3*g_3(range),type = 'l')

###Question 4
#construct standard normal want to draw from normal so set g
g_n<-function(x){
  first<-1/sqrt(2*pi)
  second<-exp(-x^2/2)
  first*second
}

h<-function(x){
  0.5*exp(-x) # only need to consider half becuase it is symetric
}
range<-seq(-3,3,0.01)
M=1.5
plot(range,g_n(range),type = 'l',ylim = c(0,1))
lines(range,M*h(range))
#----find M such that g(x)<=Mh(x)


inv_cdf_mh_p<-function(x){
  -log(2-2*x) #x is from unif[0,1]
  #-log(x)
  #-log(1-1*x)
}

run_4<-0
s_4<-rep(0,100)
x_select_4<-c()
y_select_4<-c()
for (i in 1:length(s_4)) {
  repeat{
    run_4<-run_4+1
    x<-inv_cdf_mh_p(runif(1,0,1))
    y<-runif(1,0,M*h(x))
    if( 0<=y&y<=g_n(x)){break}
  }
  x_select_4<-c(x_select_4,x)
  y_select_4<-c(y_select_4,y)
}

# reject/accept
rej_act<-(run_4-100)/100
rej_act
#rej_act = 0.72
p_reject<-1-100/run_4
p_accpet <- 100/run_4
expected<-p_reject/p_accpet
expected
#expected = 0.72




# box muler
u_1<-runif(100)
u_2<-runif(100)
x_box_muller<-sqrt(-2*log(u_1))*cos(2*pi*u_2)
y_box_muller<-sqrt(-2*log(u_1))*sin(2*pi*u_2)
plot(x_box_muller,y_box_muller)
qqplot(y_box_muller,x_select_4,main = "QQ plot")


# Question 5
g_5<-function(x,y){
  x*exp(x*y)
}
run_5<-0
f5_x<-rep(0,1000)
x_select5<-c()
y_select5<-c()

for (i in 1:length(f5_x)) {
  repeat{
    x<-runif(1,1,2)
    y<-runif(1,0,1)
    g_x_y<-runif(1,1,g_5(2,1))
    run_5<-run_5+1
    if( 0<=g_x_y & g_x_y<=g_5(x,y)){break}
  }
  x_select5<-c(x_select5,x)
  y_select5<-c(y_select5,y)
}
plot(x_select5,y_select5)


###appendix for different approach
# q2

try_x<-runif(100000,-1,1)
try_x<-data.frame(try_x)
try_y<-runif(100000,0,4.5)
try_y<-data.frame(try_y)
i=0
run<-0
x_select_2<-c()
y_select_2<-c()
while (i<1000) {
  run<-run+1
  x<-sample_n(try_x,1)$try_x
  y<-sample_n(try_y,1)$try_y
  if(((x > -1 & x<0)| (x>0.5 &x<1)) & 0<=y & y<=g(x) ){
    x_select_2<-c(x_select_2,x)
    y_select_2<-c(y_select_2,y)
    i=i+1
  }
  
}

plot(x_select_2,y_select_2)