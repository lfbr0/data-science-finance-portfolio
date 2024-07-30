rm(list=ls(all=TRUE))

### Reading the data files ###

portfolio=read.table("autodata.txt",header=TRUE,sep=";")#choose autodata.txt
names(portfolio)
nrow(portfolio)
# The popdensity and region variables were used to define the zone. 
# We will not used them in the tariff

claims=read.table("claimsdata.txt",header=TRUE,sep=";")  #choose claimsdatanew
names(claims)
nrow(claims)


### The study will rely only on the Third Party Liability coverage ###

TPclaims=claims[claims$coverage=="1RC",]
nrow(TPclaims)

### Organizing the files data ###

### Number of Claims per Policy ###

# Counting the number of claims for each policy
T=table(TPclaims$ncontract)
T1=as.numeric(names(T))
T2=as.numeric(T)
n1 = data.frame(ncontract=T1,nclaims=T2)
I = portfolio$ncontract%in%T1
T1=portfolio$ncontract[I==FALSE]
n2 = data.frame(ncontract=T1,nclaims=0)
number=rbind(n1,n2)
table(number$nclaims)

### Organize data in appropriate files for modeling Frequency and Severity of Claims ###

# Frequency
baseFREQ = merge(portfolio,number)
head(baseFREQ)
nrow(baseFREQ)

#remove outlier in nclaims
baseFREQ <- subset(baseFREQ, subset=nclaims<max(baseFREQ$nclaims))
head(baseFREQ)
nrow(baseFREQ)


N<-baseFREQ$nclaims 
E<-baseFREQ$exposition 

# Severity
baseSEV=merge(portfolio,TPclaims) 
tail(baseSEV)
nrow(baseSEV)
baseSEV=baseSEV[baseSEV$cost>0,]
nrow(baseSEV)

#seperate baseSEV into common and large claims
limSup = 5060
baseSEV_common = baseSEV[baseSEV$cost<=limSup,]
baseSEV_large = baseSEV[baseSEV$cost>limSup,]

library(fitdistrplus)
library(MASS)
library(vcd)
library(goft)

##### FITTING FOR NCLAIMS

#Test for Poisson
fitdistr(baseFREQ$nclaims,"poisson")
model.poisson=goodfit(baseFREQ$nclaims,type="poisson",method="ML")
model.poisson
summary(model.poisson)
plot(model.poisson, xlab="N? of Claims", ylab="Sqrt(Frequency)",
     main="Fitting a Poisson distribution")


#Test for Negative Binomial Distribution
fitdistr(baseFREQ$nclaims,"negative binomial")
model.nb=goodfit(baseFREQ$nclaims,type="nbinomial",method="ML")
model.nb
summary(model.nb) #if p value > alpha (1 - confidence level), no reject H0

#We cannot reject the posibility that this is a Negative Binomial distribution
plot(model.nb, xlab="N? of Claims", ylab="Sqrt(Frequency)",
     main="Fitting a Negative Binomial distribution")


##### FITTING FOR SEVERITY/AMOUNT COMMON CLAIMS
library(fitdistrplus)
descdist(baseSEV_common$cost)

gamma_fit(baseSEV_common$cost)
gamma_test(baseSEV_common$cost)

lnorm_test(baseSEV_common$cost)

ig_fit(baseSEV_common$cost)
ig_test(baseSEV_common$cost) 

gp_fit(baseSEV_common$cost, method="amle")
gp_test(baseSEV_common$cost,B=999) 

weibull_test(baseSEV_common$cost)

beta_fit <- fitdist(baseSEV_common$cost/limSup, "beta")
shape1_hat <- beta_fit$estimate[1]
shape2_hat <- beta_fit$estimate[2]
ks_test <- ks.test(baseSEV_common$cost/limSup, "pbeta", shape1_hat, shape2_hat)
ks_test

if (!require(ADGofTest)) install.packages("ADGofTest", dependencies=TRUE)
library(ADGofTest)
ad_test <- ADGofTest::ad.test(baseSEV_common$cost/limSup, null = "pbeta", shape1 = shape1_hat, shape2 = shape2_hat)
ad_test

library(fitdistrplus)
library(goft)
shape_mme <- gamma_fit(baseSEV_common$cost)[1]
rate_mme <- gamma_fit(baseSEV_common$cost)[2]
# Fit Gamma distribution using MME estimates
gamma_fit_mme <- fitdist(baseSEV_common$cost, "gamma", method = "mme", start = list(shape = shape_mme, rate = rate_mme))
# Summary of the fitted distribution
summary(gamma_fit_mme)
# Plotting ECDF and fitted Gamma distribution
plot(ecdf(baseSEV_common$cost), col = "blue",
     main = "Fitting Gamma Distribution (MME) to Common Claims Amount")
curve(pgamma(x, shape = gamma_fit_mme$estimate["shape"], rate = gamma_fit_mme$estimate["rate"]), 
      from = min(baseSEV_common$cost), to = max(baseSEV_common$cost), 
      add = TRUE, col = "red", lwd = 2)
# Add legend
legend("bottomright", legend = c("Empirical", "Gamma Fit (MME)"),
       col = c("blue", "red"), lty = 1, lwd = 2)
# Goodness-of-fit tests
gof_gamma_mme <- gofstat(gamma_fit_mme)
print(gof_gamma_mme)


# Fit Log-normal distribution
lnorm_fit <- fitdist(baseSEV_common$cost, "lnorm")
lnorm_test <- gofstat(lnorm_fit)
# Plot Log-normal distribution against empirical data
plot(ecdf(baseSEV_common$cost), col = "blue", main = "Fitting Log-normal Distribution to Common Claims Amount")
curve(plnorm(x, meanlog = lnorm_fit$estimate[1], sdlog = lnorm_fit$estimate[2]), add = TRUE, col = "red", lwd = 2, n = 100)
legend("bottomright", legend = c("Empirical", "Log-normal Fit"),
       col = c("blue", "red"), lty = 1, lwd = 2)


# Fit Weibull distribution
weibull_fit <- fitdist(baseSEV_common$cost, "weibull")
weibull_test <- gofstat(weibull_fit)
# Plot Weibull distribution against empirical data
plot(ecdf(baseSEV_common$cost), col = "blue", main = "Fitting Weibull Distribution to Common Claims Amount")
curve(pweibull(x, shape = weibull_fit$estimate[1], scale = weibull_fit$estimate[2]), add = TRUE, col = "red", lwd = 2, n = 100)
legend("bottomright", legend = c("Empirical", "Weibull Fit"),
       col = c("blue", "red"), lty = 1, lwd = 2)


##### SEVERITY/AMOUNT LARGE CLAIMS

large_claims_mean = mean(baseSEV_large$cost)
print(paste("Large Claims Cost Mean ->", large_claims_mean))
large_claims_std = sd(baseSEV_large$cost)
print(paste("Large Claims Cost Std Dev ->", large_claims_std))

#plot histogram
hist(baseSEV_large$cost, main = "Histogram of Large Claims Cost")

#plot boxplot
boxplot(baseSEV_large$cost, main = "Boxplot of Large Claims Cost", horizontal = TRUE)

#Fit pareto dist
library(VGAM)
pareto_fit <- fitdist(baseSEV_large$cost, "pareto", start = list(shape = 2, scale = limSup))
gofstat(pareto_fit)
#we can see that the p-values are above 0.05, indicate it follows a pareto dist.

alpha = pareto_fit$estimate["shape"]
m = pareto_fit$estimate["scale"]
pareto_mean = m*alpha/(alpha-1)
print(paste("Large Claims Mean Cost with Pareto dist. ->", pareto_mean))

