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

#################### Claims Frequency GLM (End of data preparation) #########################

library(MASS)

#Start by analyzing which numerical columns could be dropped to avoid

useless_freq_columns = c("nclaims", "exposition", "bonus", "region", "popdensity", "ncontract")
numeric_freq_columns <- names(baseFREQ[, sapply(baseFREQ, is.numeric) & !names(baseFREQ) %in% useless_freq_columns])

glm_test <- function(formula) {
  print(summary(glm.nb(formula, data = baseFREQ, link = log)))
}

glm_model <- function(formula) {
  model <- glm.nb(formula, data = baseFREQ, link = log)
  return(model)
}

for (name in numeric_freq_columns) {
  if (name %in% useless_freq_columns) next #skip these columns
  
  formula <- as.formula(paste("nclaims ~", name, "+ offset(exposition)"))
  glm_test(formula)
}

#analyzing the above results, we could consider dropping agevehicle & power

#use ANOVA testing

library(dplyr)

#equivalent of drop column [inplace=False], leaving it here for reference
#baseFREQ %>% select(-one_of("bonus"))


useless_freq_columns = c("exposition", "bonus", "region", "popdensity", "ncontract", "agevehicle")
head(baseFREQ %>% select(-one_of(useless_freq_columns)))


#now for the remaining columns, we can categorize them and then decide on aggregations, standard insured, etc

#starting with agredriver, we saw through the initial EDA that lower ages tend to have more claims
#so it follows that the lower age intervals should be shorter...
agedriver_levels <- c(18,22,25,28,31,41,51,61,71,81,101)
baseFREQ$agecut <- cut(baseFREQ$agedriver,breaks=agedriver_levels,right=FALSE)

useless_freq_columns <- append(useless_freq_columns, "agedriver")
head(baseFREQ %>% select(-one_of(useless_freq_columns)))

#convert power & brand to factors/levels
baseFREQ$power <- as.factor(baseFREQ$power)
baseFREQ$brand <- as.factor(baseFREQ$brand)
baseFREQ$zone <- as.factor(baseFREQ$zone)
baseFREQ$fuel <- as.factor(baseFREQ$fuel)

head(baseFREQ %>% select(-one_of(useless_freq_columns)))

###with categorical variables, we can now test out the possibility of feature removing
##start by checking the two with high p-values: agevehicle & power
#agevehicle
agevehicle_lev<-c(0,4,11,16,20,101)
baseFREQ$vehcut<-cut(baseFREQ$agevehicle,breaks=agevehicle_lev,right=FALSE)
model_full <- glm_model("nclaims~zone+power+vehcut+agecut+brand+fuel+offset(log(exposition))")
model_no_age_vehicle <- glm_model("nclaims~zone+power+agecut+brand+fuel+offset(log(exposition))")
#perform a model 1 vs model 2 test. If low p-value, then model 2 is better
anova(model_no_age_vehicle,model_full,test="Chisq")
#low p-value, so model 2 is better, we should include vehcut

#power
model_no_pow <- glm_model("nclaims~zone+agecut+brand+fuel+vehcut+offset(log(exposition))")
anova(model_no_pow,model_full,test="Chisq")
#high p-value, indicating that power does not hold much statistical significance, we can remove it
useless_freq_columns <- append(useless_freq_columns, "power")

###start analyzing possible groupings and standard insured by feature
head(baseFREQ %>% select(-one_of(useless_freq_columns)))


#install.packages("multcomp") #if error on library(multcomp)
library(multcomp)

##zone
glm_test("nclaims ~ zone + offset(log(exposition))")
#performing this simple glm test, we can see that zone B holds no significance
#on its own, and can be aggregated with standard insured
glm_test("nclaims ~ relevel(baseFREQ$zone,\"C\") + offset(log(exposition))")
#checking for C as standard insured, we can see no groupings
#this whole process is too long, better way is using GLH (Generalized Linear Hypothesis)
#High p-value means aggregate
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(zone="Tukey")))

#analyzing this we can see that B-A are almost a perfect match, we will then aggregate them
levels(baseFREQ$zone) <- c(levels(baseFREQ$zone), "A;B")
baseFREQ$zone[baseFREQ$zone %in% c("A", "B")] <- "A;B"

#by default, zone C is standard insured. And it's okay since it's the most predictive var
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(zone="Tukey")))


##fuel
summary(glht(full_model, mcp(fuel="Tukey")))
#can not be aggregated, standard insured is fuel D (Diesel) which is more predictive


##brand
summary(glht(full_model, mcp(brand="Tukey")))
#analzying the output & the previous EDA, we could reasonabily infer that 10 and 11 should be together
levels(baseFREQ$brand) <- c(levels(baseFREQ$brand), "10;11")
baseFREQ$brand[baseFREQ$brand %in% c("10", "11")] <- "10;11"
#lets see the groupings again
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
glm_test("nclaims ~ brand + offset(log(exposition))")

#we can see an interesting thing, group 1 is standard insured but it's suggested that brand 4 is agglomerated with brand 1
summary(glht(full_model, mcp(brand="Tukey")))

#and in fact, there is no real difference between 1 and 4, so we'll rebrand brand 4 to 1 (standard insured)
baseFREQ$brand[baseFREQ$brand %in% c("1", "4")] <- "1"

#lets reanalyze the groupings
glm_test("nclaims ~ brand + offset(log(exposition))")
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(brand="Tukey")))

#another possible grouping is 10;11 and 13. They all present a similar risk profile and the p-values seem to suggest it as well
levels(baseFREQ$brand) <- c(levels(baseFREQ$brand), "10;11;13")
baseFREQ$brand[baseFREQ$brand %in% c("10", "11", "13", "10;11")] <- "10;11;13"

#an interesting grouping, that can be infered from the graph is 3 & 5
glm_test("nclaims ~ relevel(baseFREQ$brand, \"3\") + offset(log(exposition))")
#in fact, the p-value is 0.9252, raising the very good possibility of joining them
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(brand="Tukey")))
#again proved by the p-value above, lets join them together
levels(baseFREQ$brand) <- c(levels(baseFREQ$brand), "3;5")
baseFREQ$brand[baseFREQ$brand %in% c("3", "5")] <- "3;5"

glm_test("nclaims ~ brand + offset(log(exposition))")
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(brand="Tukey")))
#for now, let's stop since we have reduced the model sufficiently


##agecut
glm_test("nclaims ~ agecut + offset(log(exposition))")
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(agecut="Tukey")))
#being that the standard insured is [18,22), there is the proposal that the 22-25 age bracket
#can be grouped together since the difference p-value is 0.676274
#in fact, the second output also presents with a high p-value. We will group them together
#since it makes sense that up from 18 to 25 the risk profile is similar
levels(baseFREQ$agecut) <- c(levels(baseFREQ$agecut), "[18,25)")
baseFREQ$agecut[baseFREQ$agecut %in% c("[18,22)", "[22,25)")] <- "[18,25)"
#lets make the 18-25 the standard insured since 18-25 is the most exposed profile
baseFREQ$agecut <- relevel(baseFREQ$agecut, "[18,25)")

glm_test("nclaims ~ relevel(baseFREQ$agecut,\"[25,28)\") + offset(log(exposition))")
#relevling to 25-28 as standard insured, it comes the possibility of joining with
#the age bracket of 28-31, being that their difference is low (p-value = 0.78378)
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(agecut="Tukey")))
#indeed the Tukey Contrasts reveal the same, so we'll group them
levels(baseFREQ$agecut) <- c(levels(baseFREQ$agecut), "[25,31)")
baseFREQ$agecut[baseFREQ$agecut %in% c("[25,28)", "[28,31)")] <- "[25,31)"

glm_test("nclaims ~ agecut + offset(log(exposition))")
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(agecut="Tukey")))
#there are more possible groupings, namely the [31,51] age bracket, the 51-71 age bracket, etc.
#however, it's in 20 year gaps, which given is too much (a generation is about 20 years)
#we would be joining completely different generations, so we'll stop here


##vehcut
glm_test("nclaims ~ vehcut + offset(log(exposition))")
full_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
summary(glht(full_model, mcp(vehcut="Tukey")))
#a possible grouping would be 16-101, but as seen in the EDA there is a drop off in exposition
#starting by about vehicle age of ~ 20 years, so it might not be a very good grouping. We'll leave this as is.


##finally we have the finalized nclaims model
full_nclaims_model <- glm_model("nclaims ~ zone + brand + fuel + agecut + vehcut + offset(log(exposition))")
print(summary(full_nclaims_model))

##we can export the nclaims model
results_nclaims_model <- summary.glm(full_nclaims_model)$coefficients
write.table(results_nclaims_model,"./out/coef_nclaims.txt",append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"))


#################### Common Claims GLM #########################

###Prepare data & functions
head(baseSEV_common)
useless_sev_columns <- c("ncontract", "exposition", "region", "bonus", "popdensity", "coverage", "n")
numeric_freq_columns <- names(baseSEV_common[, sapply(baseSEV_common, is.numeric) & !names(baseSEV_common) %in% useless_freq_columns])

glm_sev_model <- function(formula) {
  model <- glm(formula, family=Gamma(link="log"), data=baseSEV_common)
  return(model)
}

glm_sev_test <- function(formula) {
  model <- glm_sev_model(formula)
  print(summary(model))
}


###Start by analyzing what variables we could possibly remove from GLM reg.
head(baseSEV_common %>% select(-one_of(useless_sev_columns)))
for (name in names(baseSEV_common)) {
  if (name %in% useless_sev_columns) next #skip these columns
  formula <- as.formula(paste("cost ~", name))
  glm_sev_test(formula)
}
#interestingly, almost all features exceed the threshold of 0.05. This
#doesn't mean we will remove all of them, but we can eliminate more variables
#for this regression than for the nclaims GLM model.
#let's categorize them in the same manner as we did for the nclaims
#in order to form a compatible Pricing Structure

#convert power & brand to factors/levels
baseSEV_common$power <- as.factor(baseSEV_common$power)
baseSEV_common$brand <- as.factor(baseSEV_common$brand)
baseSEV_common$zone <- as.factor(baseSEV_common$zone)
baseSEV_common$fuel <- as.factor(baseSEV_common$fuel)

#same zone groupings as zone freq
levels(baseSEV_common$zone) <- c(levels(baseSEV_common$zone), "A;B")
baseSEV_common$zone[baseSEV_common$zone %in% c("A", "B")] <- "A;B"

#same brand groupings
#10 & 11
levels(baseSEV_common$brand) <- c(levels(baseSEV_common$brand), "10;11")
baseSEV_common$brand[baseSEV_common$brand %in% c("10", "11")] <- "10;11"
#1 & 4
baseSEV_common$brand[baseSEV_common$brand %in% c("1", "4")] <- "1"
#10, 11 & 13
levels(baseSEV_common$brand) <- c(levels(baseSEV_common$brand), "10;11;13")
baseSEV_common$brand[baseSEV_common$brand %in% c("10", "11", "13", "10;11")] <- "10;11;13"
#3 & 5
levels(baseSEV_common$brand) <- c(levels(baseSEV_common$brand), "3;5")
baseSEV_common$brand[baseSEV_common$brand %in% c("3", "5")] <- "3;5"

#categorize agedriver & same groupings as nclaims
baseSEV_common$agecut <- cut(baseSEV_common$agedriver,breaks=agedriver_levels,right=FALSE)
useless_sev_columns <- append(useless_sev_columns, "agedriver")
#groupings
levels(baseSEV_common$agecut) <- c(levels(baseSEV_common$agecut), "[18,25)")
baseSEV_common$agecut[baseSEV_common$agecut %in% c("[18,22)", "[22,25)")] <- "[18,25)"
#lets make the 18-25 the standard insured since 18-25 is the most exposed profile
baseSEV_common$agecut <- relevel(baseSEV_common$agecut, "[18,25)")
#indeed the Tukey Contrasts reveal the same, so we'll group them
levels(baseSEV_common$agecut) <- c(levels(baseSEV_common$agecut), "[25,31)")
baseSEV_common$agecut[baseSEV_common$agecut %in% c("[25,28)", "[28,31)")] <- "[25,31)"

#categorize vehicle age & same groupings as nclaims
baseSEV_common$vehcut <- cut(baseSEV_common$agevehicle,breaks=agevehicle_lev,right=FALSE)
useless_sev_columns <- append(useless_sev_columns, "agevehicle")


###Determine which variables are useless
all_sev_features <- names(baseSEV_common %>% select(-one_of(useless_sev_columns)))
for (feature in setdiff(all_sev_features, c("cost"))) {
    print(paste("\n\nChecking 1v2 ANOVA Chi-Square for feature:", feature))
    full_model <- glm_sev_model("cost ~ zone + power + brand + fuel + agecut + vehcut")
    
    #determine partial model features & formula
    partial_features <- setdiff(all_sev_features, c("cost", feature))
    partial_formula <- paste("cost ~", paste(partial_features, collapse=" + "))
    partial_model <- glm_sev_model(partial_formula)
    
    #perform test
    print(anova(partial_model, full_model, test="Chisq"))
    #high p-value, remove variable; low p-value let variable stay
}
#performing anova clearly outlines that removing agecut and vehcut would be bad
#for the model, so we keep them
#fuel, however has a really p-value of 0.94, so we can remove fuel from our severity model
useless_sev_columns <- append(useless_sev_columns, "fuel")

#all_sev_features <- names(baseSEV_common %>% select(-one_of(useless_sev_columns)))
head(baseSEV_common %>% select(-one_of(useless_sev_columns)))

#next up, we can check the others: zone, power, brand
all_sev_features <- names(baseSEV_common %>% select(-one_of(useless_sev_columns)))
for (feature in setdiff(all_sev_features, c("cost"))) {
  print(paste("\n\nChecking 1v2 ANOVA Chi-Square for feature:", feature))
  full_model <- glm_sev_model("cost ~ zone + power + brand + fuel + agecut + vehcut")
  
  #determine partial model features & formula
  partial_features <- setdiff(all_sev_features, c("cost", feature))
  partial_formula <- paste("cost ~", paste(partial_features, collapse=" + "))
  partial_model <- glm_sev_model(partial_formula)
  
  #perform test
  print(anova(partial_model, full_model, test="Chisq"))
  #high p-value, remove variable; low p-value let variable stay
}

#we can also see that zone is not necessary, thus we can discard it
useless_sev_columns <- append(useless_sev_columns, "zone")
head(baseSEV_common %>% select(-one_of(useless_sev_columns)))
#the other seem like reasonable features to estimate the price
#power, brand, age of driver & age of vehicle are obvious features that 
#influence the value of a claim, so we will keep them

##with this we can conclude the model
full_sev_model <- glm_sev_model("cost ~ power + brand + agecut + vehcut")
print(summary(full_sev_model))

#export results
results_sev_model <- summary.glm(full_sev_model)$coefficients
write.table(results_sev_model,"./out/coef_sev.txt",append = FALSE, quote = TRUE, sep = ";", eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, qmethod = c("escape", "double"))

