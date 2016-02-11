####################################
#   Part 1 - Parameters & settings
####################################
library(ggplot2)
nbr_simulations<-1000
nbr_exponentials<-40
lambda<-0.2
set.seed(2534)

####################################
#   Part 2 - Data Generation
####################################
value_matrix<-matrix(ncol=nbr_exponentials,nrow=nbr_simulations)
for(i in 1:nbr_simulations){
  value_matrix[i,]<-rexp(nbr_exponentials,lambda)
}
####################################
#   Part 3 - Distribution center
####################################
mean(apply(value_matrix,1,mean))
median(as.vector(value_matrix))

####################################
#   Part 3 - Distribution variance
####################################
var(apply(value_matrix,1,mean))
mean(apply(value_matrix,1,var))
print(1/(lambda^2))
####################################
#   Part 4 - Normality check
####################################

mu<-1/lambda
sigma<-1/lambda
tmp<-as.data.frame(tmp)
colnames(tmp)<-"x"
ggplot(tmp, aes(x=x))+geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="grey",alpha=.2)+stat_function(geom = "line", fun = dnorm, arg = list(mean = mu, sd = sigma/sqrt(nbr_exponentials)),
              size = 1, colour = "red")+geom_density(alpha=.3, fill="blue")+theme_bw()
shapiro.test(tmp$x)$p.value

####################################
#   Part 5 - Coverage
####################################

