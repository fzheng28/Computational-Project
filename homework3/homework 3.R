set.seed(100)
####question 1 
unif<-runif(1000,0,1)
h_x<-function(x){
  abs(cos(x+x^2)*exp(-2*x))
}
se_calculator<-function(h,f,est,n){
  var<-sum((h/f-est)**2)/(n-1)
  s<-var**0.5
  s/(n)**0.5
}
n=1000

#a


inv_f_a<- function(x){
  -log(1-x)
}
f_a<-function(x){
  exp(-x)
}
f_a_x<-inv_f_a(unif)

f_a_y<-f_a(f_a_x)
h_a_y<-h_x(f_a_x)
mean_a<-sum(h_a_y/f_a_y)/n
se_a<-se_calculator(h_a_y,f_a_y,mean_a,n)
se_a/mean_a # useful
mean_a+qnorm(0.05)*se_a
mean_a+qnorm(0.95)*se_a


inv_f_b<- function(x){
  -1/2*log(1-x)
}
f_b<-function(x){
  2*exp(-2*x)
}

f_b_x<-inv_f_b(unif)
f_b_y<-f_b(f_b_x)
h_b_y<-h_x(f_b_x)
mean_b<-sum(h_b_y/f_b_y)/1000
se_b<-se_calculator(h_b_y,f_b_y,mean_b,n)
se_b/mean_b # useful
mean_b+qnorm(0.05)*se_b
mean_b+qnorm(0.95)*se_b


inv_f_e<- function(x){
  -1/3*log(1-x)
}
f_e<-function(x){
  3*exp(-3*x)
}
f_e_x<-inv_f_e(unif)
f_e_y<-f_e(f_e_x)
h_e_y<-h_x(f_e_x)
mean_e<-sum(h_e_y/f_e_y)/1000
se_e<-se_calculator(h_e_y,f_e_y,mean_e,n)
se_e/mean_e # useful
mean_e+qnorm(0.05)*se_e
mean_e+qnorm(0.95)*se_e


u_1<-runif(1000)
u_2<-runif(1000)
x1_box_muller<-abs(sqrt(-2*log(u_1))*cos(2*pi*u_2))
f_f<-function(x){
  2/sqrt(2*pi)*exp((-x^2)/2)
}
mean_f<-sum(h_x(x1_box_muller)/f_f(x1_box_muller))/1000
se_f<-se_calculator(h_x(x1_box_muller),f_f(x1_box_muller),mean_f,n)
se_f/mean_f # useful
mean_f+qnorm(0.05)*se_f
mean_f+qnorm(0.95)*se_f


ﬁ#### queustion 2

#part A
#generalize the Monte Carlo method to a method that uses samples 
#from more general density functions in order to estimate an integral of the form
#it is not enclosed in a box


h_muti<-function(x1,x2,x3){
  cos(x1+x2)*exp(-(x1^2+x2^2+x3^2))
}

# part a


n = 1000
x1<-runif(n,-1,1)
x2<-runif(n,-1,1)
x3<-runif(n,-1,1)
x<-x1^2+x2^2+x3^2
q1<-8*sum(h_muti(x1,x2,x3))/n

x2_1<-as.numeric(x1<=1 & x1>=0)
x2_2<-as.numeric(x2<=1 & x2>=0)
x2_3<-as.numeric(x3<=1 & x3>=0)
q2<-8*sum(x2_1*x2_2*x2_3*h_muti(x1,x2,x3))/n



8*sum(as.numeric(x<=1.1)*x2_1*x2_2*x2_3)/n





#part B

b =1/sqrt(2)
n <-1000
x1_sample<-rnorm(n, mean = 0, sd = b)
x2_sample<-rnorm(n, mean = 0, sd = b)
x3_sample<-rnorm(n, mean = 0, sd = b)

f_muti<-function(x1,x2,x3,b){
  (2*pi*b**2)^(-3/2)*exp(-(x1^2+x2^2+x3^2)/(2*b**2))
}
h_muti<-function(x1,x2,x3){
  cos(x1+x2)*exp(-(x1^2+x2^2+x3^2))
}
se_calculator<-function(h,f,est,n){
  var<-sum((h/f-est)**2)/(n-1)
  s<-var**0.5
  s/(n)**0.5
}
h_y<-h_muti(x1_sample,x2_sample,x3_sample)
f_y<-f_muti(x1_sample,x2_sample,x3_sample,b)




indicator_i1<-x1_sample^2+x2_sample^2+x3_sample^2
in1<-as.numeric(indicator_i1<=1)
estimator1<-sum((in1*h_y/f_y))/1000
estimator1+qnorm(0.05)*se_calculator(in1*h_y,f_y,estimator1,n)
estimator1+qnorm(0.95)*se_calculator(in1*h_y,f_y,estimator1,n)


in2_1<-as.numeric(x1_sample<=1 & x1_sample>=0)
in2_2<-as.numeric(x2_sample<=1 & x2_sample>=0)
in2_3<-as.numeric(x3_sample<=1 & x3_sample>=0)
in2<-in2_1*in2_2*in2_3
estimator2<-sum((in2*h_y/f_y))/1000
se_calculator(in2*h_y,f_y,estimator2,n)
estimator2+qnorm(0.05)*se_calculator(in2*h_y,f_y,estimator2,n)
estimator2+qnorm(0.95)*se_calculator(in2*h_y,f_y,estimator2,n)

sd(in2*h_y/f_y)/1000


estimator3<-sum((h_y/f_y))/1000
estimator3+qnorm(0.05)*se_calculator(h_y,f_y,estimator3,n)
estimator3+qnorm(0.95)*se_calculator(h_y,f_y,estimator3,n)



in4<-as.numeric(indicator_i1<=1.1)
estimator4<-sum((in4*in2/f_y))/1000
estimator4+qnorm(0.05)*se_calculator(in4*in2,f_y,estimator4,n)
estimator4+qnorm(0.95)*se_calculator(in4*in2,f_y,estimator4,n)




##question 3
# central limit themom
sample_bin<-rbinom(500, 30, 3/4)
estmated_np<-mean(sample_bin)
estmated_p<-estmated_np/30
a<-10
b<-20

sampling_dist<-c()
for (i in 1:1000) {
  sampling_dist<-c(sampling_dist,mean(rbinom(500, 30, estmated_p) %in% c(a:b)))
}
quantile(sampling_dist,.95)
quantile(sampling_dist,.05)


sampling_dist_non<-c()
for (i in 1:1000) {
  bsample <- mean(sample(sample_bin,500,replace=T) %in% c(a:b))
  sampling_dist_non<-c(sampling_dist_non,bsample)
  
}
quantile(sampling_dist_non,.95)
quantile(sampling_dist_non,.05)
se_calculator((rbinom(500, 30, 3/4) %in% c(a:b)),1,mean(rbinom(500, 30, 3/4) %in% c(a:b)),30)

mean(rbinom(500, 30, 3/4) %in% c(a:b)) +qnorm(0.05)*sd((rbinom(500, 30, 3/4) %in% c(a:b)))/30**0.5
mean(rbinom(500, 30, 3/4) %in% c(a:b)) +qnorm(0.95)*sd((rbinom(500, 30, 3/4) %in% c(a:b)))/30**0.5

n<-30
p<-3/4
mean(rbinom(500, 30, 3/4) %in% c(a:b)) +qnorm(0.05)*(p*(1-p))**0.5
mean(rbinom(500, 30, 3/4) %in% c(a:b)) +qnorm(0.95)*(p*(1-p))**0.5
mean(sample_bin) +qnorm(0.05)*(n*p*(1-p))**0.5
mean(sample_bin)  +qnorm(0.05)*(p*(1-p))**0.5

  
exp_sample<-rexp(500, rate = 1)**2
lamda<-1/mean(exp_sample)
exp_sampling<-c()
for (i in 1:1000) {
  new_sample <- rexp(500, rate = lamda)
  exp_new_sample<-mean(new_sample)
  exp_sampling<-c(exp_sampling,exp_new_sample)
  
}
quantile(exp_sampling,.95)
quantile(exp_sampling,.05)

sampling_expo_non<-c()
for (i in 1:1000) {
  bsample <- mean(sample(exp_sample,500,replace=T))
  sampling_expo_non<-c(sampling_expo_non,bsample)
  
}
quantile(sampling_expo_non,.95)
quantile(sampling_expo_non,.05)

#clt
#depend on sample
mean(exp_sample) +qnorm(0.05)*sd(exp_sample)/500**0.5
mean(exp_sample) +qnorm(0.95)*sd(exp_sample)/500**0.5

#close form
2+qnorm(0.05)*22^0.5/500**0.5
2+qnorm(0.95)*22^0.5/500**0.5

#####question 4

m = 300
lamda=5
question4_x<-rexp(n, rate = lamda)
expected_y_lamda<-function(n,lamda,x){
  sum((log(lamda)-lamda*x+x)*(1/lamda - x))/n
}
lamdas<-c()
step<-1
for (i in 1:2000) {
  question4_x<-rexp(m, rate = lamda)
  lamdas<-c(lamdas,lamda)
  lamda=lamda-step/i*expected_y_lamda(n,lamda,question4_x)
  
}

lamdas_1<-lamdas
lamdas_5<-lamdas
lamdas_10<-lamdas
lamdas<-cbind(lamdas_1,lamdas_5,lamdas_10)
lamdas
library(ggplot2)
library("reshape2")
test_data_long <- melt(lamdas, id="lamdas")
colnames(test_data_long)[1]='iterations'
colnames(test_data_long)[2]='step_size'

ggplot(data=test_data_long,
       aes(x=iterations, y=lamdas, colour=step_size)) +
  geom_line()
