#Survey Sampling - Assignment
library(robsurvey)

#Import data
data("workplace")
attach(workplace)
#Sample of 142 workplaces on the following vairables :
# - ID : identifier varirable
# - weight : sampling weight
# - employment : employment total
# - payroll : payroll total (1000$)
# - strat : stratum identifier
# - fpc : finite population correction
#Other ones : losdata in package survey

##Statistiques descriptives
library(stargazer)
stargazer(data.frame(workplace),
          type = "text",  
          title = "Table : Descriptive Statistics of ...", 
          digits=3,
          summary.stat = c("n","mean","sd","min","max"))

#En moyenne 66.282 employé par workplace...
#En moyenne la masse salariale est de 1 071 690$ ???
#Etc...
#Focus sur employment mais payroll peut être utile : payroll~employment
#Focus sur payroll plutôt ?
#Minimum est de 1 employé contre 661 pour le maximum donc une grande variance

library(fBasics)
basicStats(workplace)

#Min ???
#Maximum values are far from the 3rd quantile value for employment, so maybe there is extreme values or outliers.
#There is a big difference between the mean and the median (employment and payroll), the medain is smaller than the mean for each, so there are probably a lot of high observations which pulls the value of the mean up.

#Skewness mesure l'asymétrie de la distribution d'une variable.
#Skewness = 3.39 > 0 donc la distribution est décalé à gauche de la médiane.
#Il y aura une queue de distribution vers la droite
#Kurtosis est un indicateur d’aplatissement de la distibution.
#Kurtosis = 18.80 > 0. La distribution est leptokurtique , c'est-à-dire une distribution pointue en sa moyenne.
#Et des queues de distribution plus épaisses que la normale (fat tails) impliquant des valeurs abérrantes plus fréquentes.
#Kurtosis loin de 0 alors la distribution de la variable est loin de celle d'une loi normale.

#Histogram (distribution)
hist(employment, col="grey", border="black", prob = TRUE, ylab = "density", xlab = "Employment", main = "")
lines(density(employment), lwd = 2, col = "blue")
curve(dnorm(x, mean(employment), sd(employment)), col="orange", add=T, lwd=2)
legend("topright", legend=c("Estimation de la densité","Estimation de la loi normale"), lty=c(1,1), lwd=2, col=c("blue","orange"), cex=0.6)
#Orange : Estimation avec kernel de la fonction de densité d'une loi normale avec la moyenne et l'écart-type des données.
#Lien avec Skewness and Kurtosis : Confirme queue de distribution vers la droite, la distribution est asymétrique.
#Comparaison avec densité loi normale.
#Présence outlier avec 661 employés.

#Boxplot
boxplot(employment)
boxplot(payroll)
#Outside the mustaches we can see atypical observations

#Scatterplot
pairs(workplace[3:4], upper.panel = NULL)
#Again some atypical observations.

##Univariate analysis
#K-sigma rule
#Standardization of the data
df.std <- scale(workplace[3:4], center = TRUE, scale = TRUE)
boxplot(df.std)
#Outside the mustaches we can see atypical observations

summary(df.std)
#Min < |3| so no problem but Max > |3| for both.
#Min close to 1st quantile so ok but Max far from 3rd quantile.
#Median < Mean so high observation pull up the mean.

#Determine the observations which are distant to more than k
#Are all outliers outlying observations on both variables ?
#Choose the value k
k = 3

#Identification of the observations with extreme values
outliers.tab <- list()
int_var <- c("employment", "payroll")
for (i in int_var){
  outliers.tab[[i]] <- which(abs(df.std[,i])>=k)
}
outliers.tab
#2 outliers for employment and 4 for payroll.

#To sum up the results
outliers.k.sigma.allVar <- unlist(outliers.tab)
#Are some observations identified as outliers on several variables ?
table(outliers.k.sigma.allVar)
#Yes the observations 2 for both variables is an outlier.

#Tag the outlying observations on the scatterplot of the data.
#Illustration on a plot of which observations are outlying
#Keep only unique observations
outliers.k.sigma <- sort(unique(outliers.k.sigma.allVar))
#Plot
plot(workplace[, int_var[1:2]])
points(workplace[outliers.k.sigma, int_var[1:2]], col = "red", pch = 18)

#Plot the histograms of the variables.
hist(df.std[, 1], main = "Employment")
hist(df.std[, 2], main = "Payroll")
#Still asymetric distribution on the right

#Under the Gaussian assumption and with standardized variables, what is the probability of observations to be contained in [−k,k] ?
prob <- pnorm(k)-pnorm(-k)
prob 
#Under the Gaussian assumption and with standardized variables, the probablity of observations to be contained in [-3,3] is 99,73%.

#What is the theoretical k which ensures to contain 95% of the observations ?
alpha <- 0.05
val.cut <- qnorm(1-alpha/2)
val.cut

#You can change the k value or the alpha value
#Computing the probability of observations based on a k 
k <- 1.96
prob <- 1-2*pnorm(-k)
prob 

#Computing the k based on an alpha value
alpha <- 0.05
k <- qnorm(1-alpha/2)
k

#Global plot
par(mfrow=c(1,2))
for (i in int_var){
  hist(df.std[,i], main = i)
  abline(v = c(-k,k), col = "red")
}
par(mfrow=c(1,1)) 
#We can see on each plot that all the observations on the right of the red vertical line are outliers.

#On robustly standardized data
#Standardize the data with the median and the IQR estimators.
#... do it or not ? more outliers or not ?

#The Grubbs’ test
library(outliers)
grubbs.test(employment)
#We got p-value < 5% : we can reject the null hypothesis, so the highest value is an outlier.

grubbs.test(employment, opposite = TRUE)
#We got p-value > 5% : so can't reject the null hypothesis that the lowest value is not an outlier.

grubbs.test(payroll)
#We got p-value < 5% : we can reject the null hypothesis, so the highest value is an outlier.

grubbs.test(payroll, opposite = TRUE)
#We got p-value > 5% : so can't reject the null hypothesis that the lowest value is not an outlier.

##Mutivariate analysis
#Graphical representation of location and scatter estimators
#Represent graphically the mean estimate.
mu = colMeans(workplace[,int_var], na.rm = TRUE)
plot(workplace[,int_var[1:2]])
points(mu[1], mu[2], pch = 19, col = "red", cex=2)

#Represent graphically the scatter estimate with an ellipse (the code of the ellips function is given in appendix).
# From the tolEllipsePlot function from rrcov
# Arguments:
# - loc: location estimate
# - cov: scatter estimate
ellips <- function(loc, cov) {
  dist <- sqrt(qchisq(0.975, 2))
  A <- solve(cov)
  eA <- eigen(A)
  ev <- eA$values
  lambda1 <- max(ev)
  lambda2 <- min(ev)
  eigvect <- eA$vectors[, order(ev)[2]]
  z <- seq(0, 2 * pi, by = 0.01)
  z1 <- dist/sqrt(lambda1) * cos(z)
  z2 <- dist/sqrt(lambda2) * sin(z)
  alfa <- atan(eigvect[2]/eigvect[1])
  r <- matrix(c(cos(alfa), -sin(alfa), sin(alfa), cos(alfa)),
              ncol = 2)
  t(loc + t(cbind(z1, z2) %*% r))
}

#Compute the mean and the variance-covariance matrix
mu = colMeans(workplace[,int_var[1:2]], na.rm = TRUE)
Sigma = var(workplace[,int_var[1:2]], na.rm = TRUE)

#Representation
plot(workplace[,int_var[1:2]])
z1 <- ellips(loc = mu, cov = Sigma)
points(z1, type = "l")
#Interpretation : under the gaussianity assumption, we expect to have around 95% of the observations inside the ellipse.
#So all observations outside the ellipse are outliers.

##Estimation
#Population Size
N <- dim(workplace)[1]

#Total Y
total_y <- sum(employment)
total_y

#Variance S^2_y
S2_y <- var(employment)
S2_y

#Sample Size
n = 16

#Number of groups  : m > n/2
#Voir conditions sur formation groupes.

#HT with SRSWOR design
#Look at computer Lab 1
#Number of samples ??
#One without outliers
#One with oultiers
#Libraries
library(survey)
library(sampling)
#Draw sample of 16
si <- srswor(n, N)

ech.si <- svydesign(id = ~ ID, weights = weight, fpc = fpc,
                    data = workplace[which(si == 1), ])
est <- svytotal( ~ employment, ech.si)
attributes(est)

#Try with weights = rep(N/n, n) and fpc = rep(N, n)
ech.si <- svydesign(id = ~ ID, weights = rep(N/n, n), fpc = rep(N, n),
                    data = workplace[which(si == 1), ])
est <- svytotal( ~ employment, ech.si)
attributes(est)

#Estimation of Y
est

#Estimated Standard Deviation and Variance
SE(est)
SE(est)^2

#Coefficient of Variation
SE(est)/est[1]

#Confidence Interval
est[1]-1.96 * SE(est)
est[1]+1.96 * SE(est)

#HT
s1 <- svydesign(ids = ~ID, weights = ~weight,
                data = workplace)
s1

ht <- weighted_total(employment, weight, na.rm = FALSE)
ht

#RHT
weighted_total_huber(employment, weight, k = 7, type = "rht", asym = TRUE, na.rm = FALSE)
rht <- svytotal_huber(~employment, design = s1 , k = 7, type = "rht", asym = TRUE, na.rm = FALSE)
summary(rht)

#MER
mer(rht)

#MSE
#HT MSE
#Problem here...
ht_mse <- (1/N)*(total_y - ht)^2 
ht_mse

#RHT MSE
rht_mse <- (1/N)*(total_y - rht$estimate)^2 
rht_mse
mse(rht)

#MER MSE
mer_mse <- (1/N)*(total_y - mer(rht)$estimate)^2 
mer_mse
mse(mer(rht))

#Efficency
rht_eff <- ht_mse/rht_mse 
rht_eff

mer_eff <- ht_mse/mer_mse 
mer_eff





#Clement method
#Draw sample with an outlier
N <- dim(workplace)[1]
n <- 20 #Sample size
sum(employment) #Y total
var(employment) #Variance S^2_y

s1 <- srswor(n-1, N)
s1

df_s1 = workplace[which(s1 == 1), ]
df_s1

#Add an outlier
outlier_ind <- which(workplace$employment == max(employment))
outlier_ind
s1[n] = outlier_ind

df_s1_ <- data.frame(df_s1, workplace[2, ]) #Not this
#rbind() and nrow()

#Plot sample scatterplot with outlier in red
#Look Code R Clément

#HT
#Computer Lab 1

#Winsorized HT
svytotal_k_winsorized(x, design, k, na.rm = FALSE, trim_var = FALSE)

#MSE and efficiency

#Same things for a sample without outliers







