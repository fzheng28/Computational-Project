# Q1
n=1000
b =1/2
inv_cdf_mh_p<-function(x){
  if(x >= 0.5){
    w = -log(2-2*x)}
  if(x < 0.5){
    w = log(2*x)
    }
  x1_sample_list<-c(x1_sample_list,-w)
}
x1_sample<-c()
for (i in 1:n) {
  x1_sample<-c(x1_sample,inv_cdf_mh_p(runif(1)))
}
x2_sample<-rnorm(n, mean = 0, sd = b)

f_muti<-function(x1,x2){
  exponentional<- (1/2*exp(-abs(x1)))
  normal_dist<- (1/sqrt(2*pi*b**2)*exp(-x2^2/(2*b**2)))
  normal_dist*exponentional
}

h_muti<-function(x1,x2){
  A =as.numeric(x1^2+x2>=0)
  cos(x1+x2)*exp(-(abs(x1)+2*x2^2))*A}

mean_a<-mean(h_muti(x1_sample,x2_sample)/f_muti(x1_sample,x2_sample))

####
aux_p<-function(x1, x2){
  part1<- (-abs(x1))
  part2<-(-2*x2^2)
  exp(part1+part2)
}

q1_mcmc<-function(x1_init,x2_init,step_size, iteration){
  x1<-x1_init
  x2<-x2_init
  x1_all<-c(x1)
  x2_all<-c(x2)
  p_1<-aux_p(x1,x2)
  
  for (i in 1:iteration) {
    y1<-rnorm(1,x1,step_size)
    y2<-rnorm(1,x2,step_size)
    p_2<-aux_p(y1,y2)
    accept_prob<-min(1,p_2/p_1)
    u =  runif(1)
    if( accept_prob >= u){
      x1<- y1
      x2<- y2
      p_1<-p_2
      x1_all<-c(x1_all, x1)
      x2_all<-c(x2_all, x2)
    }
    
    if( accept_prob < u){
      x1<- x1
      x2<- x2
      p_1<-p_1
      x1_all<-c(x1_all, x1)
      x2_all<-c(x2_all, x2)
    }
  }
  
  return(cbind(x1_all,x2_all))
}

expect<-function(x1,x2){
  cos(x1+x2)*as.numeric(x1^2+x2>=0)
}

x_input1<-1
x_input2<-2
n=1000
a<-q1_mcmc(x_input1,x_input2,1,n)


z= 2 * sqrt(pi/2)
mean(expect(a[,'x1_all'],a[,'x2_all']))*z


##q2

q2_target<-function(x,s1,s2,g1){
  part1<-exp(-x^2/(2*s1^2))
  part2<-exp(-(x-g)^2/(2*s2^2))
  part1 + part2
}

gradient_p<-function(x,s1,s2,g1){
  # outter dervative for log
  part1 <- 1/q2_target(x,s1,s2,g1)
  
  # inner dervative 
  part2_1 <- (-x/s1^2)*exp(-x^2/(2*s1^2))
  part2_2 <- exp(-(x-g1)^2/(2*s2^2))*(-(x-g1)/s2^2)
  part2<- part2_1 + part2_2
  
  #together
  part1*part2
}

propose_a_given_b<- function(a,b,step_size,s1,s2,g1){
  part1<-1/sqrt(2*pi*2*step_size^2)
  inner_part2_1<- (a-(b+step_size^2*gradient_p(b,s1,s2,g1)))^2
  inner_part2_2<- 2*2*step_size^2
  part2<- exp(-inner_part2_1/inner_part2_2)
  part1*part2
}

q2_mala<-function(x1_init,s1,s2,g1,step_size,iteration){
  x1_all_2<-c(x1_init)
  x1<-x1_init
  p_1<-q2_target(x1,s1,s2,g1)
  
  accept_mala = 0
  for (i in 1:iteration) {
    y1<-rnorm(1,x1+gradient_p(x1,s1,s2,g1)*step_size^2, 2*step_size^2)
    p_2<-q2_target(y1,s1,s2,g1)
    accept_prob<-min(1,(p_2*propose_a_given_b(x1,y1,step_size,s1,s2,g1))/(p_1*propose_a_given_b(y1,x1,step_size,s1,s2,g1)))
    u<-runif(1)
    
    if( accept_prob >= u){
      accept_mala = accept_mala+1
      print(accept_mala)
      x1 <- y1
      p_1 <-p_2
      x1_all_2<-c(x1_all_2, x1)
    }
    if( accept_prob < u){
      x1 <- x1
      p_1<-p_1
      x1_all_2<-c(x1_all_2, x1)
    }
    
  }
  
  return(x1_all_2)
}

q2_random_walker<-function(x1_init,s1,s2,g1,step_size,iteration){
  x1_all_2<-c(x1_init)
  x1<-x1_init
  p_1<-q2_target(x1,s1,s2,g1)
  
  for (i in 1:iteration) {
    y1<-rnorm(1,x1, step_size)
    p_2<-q2_target(y1,s1,s2,g1)
    accept_prob<-min(1,(p_2)/(p_1))
    u<-runif(1)
    if( accept_prob >= u){
      accept_random_walker = accept_random_walker+1
      print(accept_random_walker)
      x1<- y1
      p_1<-p_2
      x1_all_2<-c(x1_all_2, x1)
    }
    if( accept_prob < u){
      x1<- x1
      p_1<-p_1
      x1_all_2<-c(x1_all_2, x1)
    }}
  return(x1_all_2)}

s1<-10
s2<-1
g<-7
step_size<-4
n2<-10^5
init_point<-2
b<-q2_mala(init_point,s1,s2,g,step_size, n2)
# accpted points: 31761
c<-q2_random_walker(init_point,s1,s2,g,step_size, n2)
# accpted points: 83864

b<-as.data.frame(b)
c<-as.data.frame(c)

library(ggplot2)
ggplot(data=b, aes(x=1:length(b), y=b, group=1)) +
  geom_line()+ theme_classic()+
  labs(title="Mala",x="Iteration", y = "S_k")

hist(b$b,main = "Mala",breaks = 50)


ggplot(data=c, aes(x=1:length(c), y=c, group=1)) +
  geom_line()+ theme_classic()+
  labs(title="Random Walker",x="Iteration", y = "S_k")

hist(c$c,main = "Random Walker",breaks = 50)

###

total = c()

for (x1 in 1:2) {
  for (x2 in 1:2) {
    for (x3 in 1:2) {
      for (x4 in 1:2) {
        t<-((x1**x2)*(x2**x3)*(x3**x4)*(x4**x1))
        total<-c(total,t)
      }
    }
  }
}

sum(total)
