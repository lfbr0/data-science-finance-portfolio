rm(list=ls(all=TRUE))
library(dplyr)

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


###################################### Fitting Logistic Regression ###########################################

useless_sev_columns <- c("ncontract", "exposition", "region", "bonus", "popdensity", "coverage", "n")

##first step is copying dataframe and making predictor var
baseSEV_normalized <- data.frame(baseSEV)
baseSEV_normalized$is_large <- baseSEV_normalized$cost > limSup

##second step is normalizing to same data types
#convert power & brand to factors/levels
baseSEV_normalized$power <- as.factor(baseSEV_normalized$power)
baseSEV_normalized$brand <- as.factor(baseSEV_normalized$brand)
baseSEV_normalized$zone <- as.factor(baseSEV_normalized$zone)
baseSEV_normalized$fuel <- as.factor(baseSEV_normalized$fuel)

#same zone groupings as zone freq
levels(baseSEV_normalized$zone) <- c(levels(baseSEV_normalized$zone), "A;B")
baseSEV_normalized$zone[baseSEV_normalized$zone %in% c("A", "B")] <- "A;B"

#same brand groupings
#10 & 11
levels(baseSEV_normalized$brand) <- c(levels(baseSEV_normalized$brand), "10;11")
baseSEV_normalized$brand[baseSEV_normalized$brand %in% c("10", "11")] <- "10;11"
#1 & 4
baseSEV_normalized$brand[baseSEV_normalized$brand %in% c("1", "4")] <- "1"
#10, 11 & 13
levels(baseSEV_normalized$brand) <- c(levels(baseSEV_normalized$brand), "10;11;13")
baseSEV_normalized$brand[baseSEV_normalized$brand %in% c("10", "11", "13", "10;11")] <- "10;11;13"
#3 & 5
levels(baseSEV_normalized$brand) <- c(levels(baseSEV_normalized$brand), "3;5")
baseSEV_normalized$brand[baseSEV_normalized$brand %in% c("3", "5")] <- "3;5"

#categorize agedriver & same groupings as nclaims
agedriver_levels <- c(18,22,25,28,31,41,51,61,71,81,101)
baseSEV_normalized$agecut <- cut(baseSEV_normalized$agedriver,breaks=agedriver_levels,right=FALSE)
useless_sev_columns <- append(useless_sev_columns, "agedriver")
#groupings
levels(baseSEV_normalized$agecut) <- c(levels(baseSEV_normalized$agecut), "[18,25)")
baseSEV_normalized$agecut[baseSEV_normalized$agecut %in% c("[18,22)", "[22,25)")] <- "[18,25)"
#lets make the 18-25 the standard insured since 18-25 is the most exposed profile
baseSEV_normalized$agecut <- relevel(baseSEV_normalized$agecut, "[18,25)")
#indeed the Tukey Contrasts reveal the same, so we'll group them
levels(baseSEV_normalized$agecut) <- c(levels(baseSEV_normalized$agecut), "[25,31)")
baseSEV_normalized$agecut[baseSEV_normalized$agecut %in% c("[25,28)", "[28,31)")] <- "[25,31)"

#categorize vehicle age & same groupings as nclaims
agevehicle_lev<-c(0,4,11,16,20,101)
baseSEV_normalized$vehcut <- cut(baseSEV_normalized$agevehicle,breaks=agevehicle_lev,right=FALSE)
useless_sev_columns <- append(useless_sev_columns, "agevehicle")
useless_sev_columns <- append(useless_sev_columns, "fuel")
useless_sev_columns <- append(useless_sev_columns, "zone")


### third step is fitting logistic regression
reglogit_formula <- as.formula("is_large ~ zone + power + vehcut + agecut + brand + fuel + offset(exposition)")
reglogit_model <- glm(reglogit_formula, data=baseSEV_normalized, family=binomial(link="logit"))
summary(reglogit_model)

# Observing the odds Ratio
exp(coefficients(reglogit_model))

# Predict the probability of reporting a large claim for a given insured profile
predict_risk_profile <- function(risk_profile) {
  print(predict(reglogit_model,newdata=risk_profile,type="response"))
}


#Compute probabilities of standard insured
get_std_insured_rp <- function() { 
  return(data.frame(zone="C", power=as.factor(4), vehcut="[0,4)", agecut="[18,25)", brand=as.factor(1), fuel="D", exposition=1)) 
}
predict_risk_profile(get_std_insured_rp())


########## PREDICTING PROBABILITIES FOR EACH FEATURE ############


#Zone D
zone_d_rp <- get_std_insured_rp()
zone_d_rp$zone = "D"
predict_risk_profile(zone_d_rp)

#Zone E
zone_e_rp <- get_std_insured_rp()
zone_e_rp$zone = "E"
predict_risk_profile(zone_e_rp)

# Zone F
zone_f_rp <- get_std_insured_rp()
zone_f_rp$zone <- "F"
predict_risk_profile(zone_f_rp)

# Zone A
zone_a_rp <- get_std_insured_rp()
zone_a_rp$zone <- "A;B"
predict_risk_profile(zone_a_rp)

# Brand 2
brand_2_rp <- get_std_insured_rp()
brand_2_rp$brand <- as.factor(2)
predict_risk_profile(brand_2_rp)

# Brand 6
brand_6_rp <- get_std_insured_rp()
brand_6_rp$brand <- as.factor(6)
predict_risk_profile(brand_6_rp)

# Brand 12
brand_12_rp <- get_std_insured_rp()
brand_12_rp$brand <- as.factor(12)
predict_risk_profile(brand_12_rp)

# Brand 14
brand_14_rp <- get_std_insured_rp()
brand_14_rp$brand <- as.factor(14)
predict_risk_profile(brand_14_rp)

# Brand 10 & 11 & 13
brand_10_11_13_rp <- get_std_insured_rp()
brand_10_11_13_rp$brand <- as.factor("10;11;13")
predict_risk_profile(brand_10_11_13_rp)

# Brand 3 & 5
brand_3_5_rp <- get_std_insured_rp()
brand_3_5_rp$brand <- as.factor("3;5")
predict_risk_profile(brand_3_5_rp)

# Fuel E
fuel_E_rp <- get_std_insured_rp()
fuel_E_rp$fuel <- as.factor("E")
predict_risk_profile(fuel_E_rp)

# Age 31-41
age_31_41_rp <- get_std_insured_rp()
age_31_41_rp$agecut <- "[31,41)"
predict_risk_profile(age_31_41_rp)

# Age 41-51
age_41_51_rp <- get_std_insured_rp()
age_41_51_rp$agecut <- "[41,51)"
predict_risk_profile(age_41_51_rp)

# Age 51-61
age_51_61_rp <- get_std_insured_rp()
age_51_61_rp$agecut <- "[51,61)"
predict_risk_profile(age_51_61_rp)

# Age 61-71
age_61_71_rp <- get_std_insured_rp()
age_61_71_rp$agecut <- "[61,71)"
predict_risk_profile(age_61_71_rp)

# Age 71-81
age_71_81_rp <- get_std_insured_rp()
age_71_81_rp$agecut <- "[71,81)"
predict_risk_profile(age_71_81_rp)

# Age 81-101
age_81_101_rp <- get_std_insured_rp()
age_81_101_rp$agecut <- "[81,101)"
predict_risk_profile(age_81_101_rp)

# Age 25-31
age_25_31_rp <- get_std_insured_rp()
age_25_31_rp$agecut <- "[25,31)"
predict_risk_profile(age_25_31_rp)

# Vehicle Age 4-11
vehcut_4_11_rp <- get_std_insured_rp()
vehcut_4_11_rp$vehcut <- "[4,11)"
predict_risk_profile(vehcut_4_11_rp)

# Vehicle Age 11-16
vehcut_11_16_rp <- get_std_insured_rp()
vehcut_11_16_rp$vehcut <- "[11,16)"
predict_risk_profile(vehcut_11_16_rp)

# Vehicle Age 16-20
vehcut_16_20_rp <- get_std_insured_rp()
vehcut_16_20_rp$vehcut <- "[16,20)"
predict_risk_profile(vehcut_16_20_rp)

# Vehicle Age 20-101
vehcut_20_101_rp <- get_std_insured_rp()
vehcut_20_101_rp$vehcut <- "[20,101)"
predict_risk_profile(vehcut_20_101_rp)

# Power 5
power_5_rp <- get_std_insured_rp()
power_5_rp$power <- as.factor(5)
predict_risk_profile(power_5_rp)

# Power 6
power_6_rp <- get_std_insured_rp()
power_6_rp$power <- as.factor(6)
predict_risk_profile(power_6_rp)

# Power 7
power_7_rp <- get_std_insured_rp()
power_7_rp$power <- as.factor(7)
predict_risk_profile(power_7_rp)

# Power 8
power_8_rp <- get_std_insured_rp()
power_8_rp$power <- as.factor(8)
predict_risk_profile(power_8_rp)

# Power 9
power_9_rp <- get_std_insured_rp()
power_9_rp$power <- as.factor(9)
predict_risk_profile(power_9_rp)

# Power 10
power_10_rp <- get_std_insured_rp()
power_10_rp$power <- as.factor(10)
predict_risk_profile(power_10_rp)

# Power 11
power_11_rp <- get_std_insured_rp()
power_11_rp$power <- as.factor(11)
predict_risk_profile(power_11_rp)

# Power 12
power_12_rp <- get_std_insured_rp()
power_12_rp$power <- as.factor(12)
predict_risk_profile(power_12_rp)

# Power 13
power_13_rp <- get_std_insured_rp()
power_13_rp$power <- as.factor(13)
predict_risk_profile(power_13_rp)

# Power 14
power_14_rp <- get_std_insured_rp()
power_14_rp$power <- as.factor(14)
predict_risk_profile(power_14_rp)

# Power 15
power_15_rp <- get_std_insured_rp()
power_15_rp$power <- as.factor(15)
predict_risk_profile(power_15_rp)

