####################################
#   Part 1 - Parameters & settings
####################################
nbr_simulations<-1000
nbr_exponentials<-40
lambda<-0.2
set.seed(1234)

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
std_values<-(apply(value_matrix,1,mean)-mean(apply(value_matrix,1,mean)))/sd(apply(value_matrix,1,mean))
std_values<-as.data.frame(std_values)
colnames(std_values)[1]<-"z"
ggplot(std_values, aes(x=z))+geom_histogram(aes(y=..density..),binwidth=.3,colour="black", fill="grey",alpha=.2)+stat_function(fun=dnorm)+geom_density(alpha=.3, fill="blue")+geom_vline(xintercept=0,col="red")+theme_bw()
shapiro.test(std_values$z)$p.value
####################################
#   Part 5 - Coverage
####################################

