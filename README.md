# covidregressions
Regressions on COVID-19 Rates
In the United States only 60% of citizens are vaccinated, and the vaccination rate has plateaued. Social scientists and statisticians are now trying to discover potential reasons someone might choose not to get vaccinated. With this code I explore the very same question: what might cause someone not to get vaccinated? There are plenty of potential reasons, one being distrust of the vaccine and the spread of misinformation which primarily relies on one’s political orientation. Statisticians however are also exploring other potential rationales such as a person's education level leading to higher rates of vaccination, or a subject's type of work, and income, all of which affect one's individual decision making. 

Another reason that should be explored is that those who remain unvaccinated have already had COVID and consider themselves immune, thus deciding they no longer need the vaccine. This code will explore the rationale mentioned above; whether having had covid affects one’s decision to get vaccinated. It is possible that after having COVID a subject no longer feels the need to get vaccinated as their immune system has already built up protection against COVID and they no longer see a reason to get the COVID-19 vaccine.


#
#Data Analysis Code (R)
 
options(scipen=999)

#Packages:

install.packages("readxl")
install.packages("writexl")

library(AER)
library(corrplot) 
library(car)
library(lmtest)
library(stargazer)
library(ggplot2)
library(GGally)
library(dplyr)
library(readxl)
library(writexl)
library(haven)

#DV:GETVAC 

#IVs w Definite correlation: EEDUC, KINDWORK, ANXIOUS, INCOME,
#IVs that would be interesting: RRACE, SEXUAL_ORIENTATION, 
#Confounding IV (decreases # of vaxxed ): HADCOVID, ACTIVITY2
 

View(hps)
head(hps)     
str(hps)     
names(hps)    
nrow(hps)  

#Clean Data:
hps.goodvars <- hps[ , c("GETVAC", "EEDUC", "KINDWORK", "ANXIOUS", "INCOME", 
                         "RRACE", "SEXUAL_ORIENTATION", "HADCOVID", "SETTING" )]
summary(hps.goodvars)

hps.clean <- na.omit(hps.goodvars)

##DESCRIPTIVE STATISTICS

#Summary of DV:
summary(hps.clean$GETVAC)

mean(hps.clean$GETVAC)
#74.5

#Summary of IV:

hadcovid_table <- table(hps.clean$HADCOVID)

##Exploring Distribution of Vars of Interest:
hist(hps.clean$GETVAC,
     col = "pink",
     breaks = 15,
     xlab = "Likelihood of Getting Vaccinated",
     main = "GETVAC Histogram")

d <- density(hps.clean$GETVAC)
plot(d, main="Density of Willingness to Get Vaccinated")
polygon(d, col="pink", border="dark red")
#many more people will definitely not be getting vaccinated than definitely will
#probably due the the fact that those surveyed had the opportunity to get vaxxed
#already and haven't, so we're looking at a skewed sample

hist(hps.clean$EEDUC,
     col = "pink",
     breaks = 15,
     xlab = "Education Level",
     main = "Level of Education of Sample Population")
#biggest category of subjects is those who have some college, then highschool
#graduates and people holding a bachelors are almost equal 
#(maybe look at only those who have some college and onwards vs those 
#under that?)

hist(hps.clean$KINDWORK,
     col = "pink",
     breaks = 6,
     xlab = "Type of Work",
     main = "Type of Work in Sample Population")
#majority of respondents work for a private company, second is selfemployed

hist(hps.clean$HADCOVID,
     col = "pink",
     breaks = 3,
     xlab = "Yes                         No                     Not Sure",
     main = "Number of participants who had Covid")
#majority of participants have not had covid (approx .5 more haven't than have)

#Correlations of Categorical Variables:

table(hps.clean$HADCOVID, hps.clean$ANXIOUS)
#majority of those who haven't had covid are also "not at all" anxious
#second biggest category is those who have had covid and are also "not at all"
#anxious

table(hps.clean$HADCOVID, hps.clean$ANXIOUS, hps.clean$INCOME)

#Checking Correlations:

cor(hps.clean$GETVAC, hps.clean$HADCOVID)

#possible control variables
all_cors <- cor(hps.clean[, c("EEDUC", "KINDWORK", "ANXIOUS", "INCOME",
                                   "RRACE", "SEXUAL_ORIENTATION", "GETVAC", 
                              "SETTING")], use = "pairwise.complete.obs")
stargazer(all_cors,
          type = "text",
          out="All_corrs.htm",
          title = "Correlations on all interval variables")
#Income and anxious have high correlation, as does  income and eeduc.
#Interesting correlations: Race is negatively correlated with eeduc, kindwork,
#                          income, and setting
#Getvac correlations of note: negative correlation with anxious.
#Potential control variables: INCOME, SETTING and ANXIOUS

#Recoding Variables of Interest as Dummies:

hadcovid_dummy <- as.factor(hps.clean$HADCOVID)
kindwork_dummy <- as.factor(hps.clean$KINDWORK)
rrace_dummy <- as.factor(hps.clean$RRACE)
sexual_orientation_dummy <- as.factor(hps.clean$SEXUAL_ORIENTATION)
setting_dummy <- as.factor(hps.clean$SETTING)


##INFERENTIAL STATISTICS

#Checking the relationships:

reg1 <- lm(GETVAC ~ EEDUC, data = hps.clean)
summary(reg1)

model0.1 <- lm(GETVAC ~ hadcovid_dummy, data = hps.clean)
summary(model0.1)

stargazer(model1,
          type = "text",
          out="Model1.htm",
          title = "Regression Output of GETVAC and HADCOVID")

reg2 <- lm(GETVAC ~ INCOME, data = hps.clean)
summary(reg2)

interaction_reg1 <- lm(GETVAC ~ EEDUC + hadcovid_dummy + EEDUC*hadcovid_dummy, 
                       data = hps.clean)
summary(interaction_reg1)

stargazer(interaction_reg1,
          type = "text",
          out="Interaction1.htm",
          title = "Interaction Regression Output of GETVAC, HADCOVID and EEDUC")

interaction_reg2 <- lm(GETVAC ~ INCOME + hadcovid_dummy + INCOME*hadcovid_dummy, 
                         data = hps.clean)
summary(interaction_reg2)

stargazer(interaction_reg2,
          type = "text",
          out="Interaction2.htm",
          title = "Interaction Regression Output of GETVAC, HADCOVID and INCOME")

#Regressions with control variables:

model1 <- lm(GETVAC ~ hadcovid_dummy, data = hps.clean)
summary(model1)
#relationship of having had covid, and not being sure are statistically 
#significant

model2 <- lm(GETVAC ~ hadcovid_dummy + INCOME, data = hps.clean)
summary(model2)

model3 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS, data = hps.clean)
summary(model3)

model4 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS, data = hps.clean)
summary(model4)

#I don't use EEDUC since INCOME has such a high correlation with it

#First regressions Model:
stargazer(model1, model2, model3, model4,
          type = "text",
          out = "firstregressions.html",
          title = "Summary of Control Variable Regressions")

stargazer(model1, model2, model3, model4, title="Regression Results",
          digits = 2,
          covariate.labels = c("Hasn't had Covid", "Unsure If They Had Covid",
                               "Income", "Health Anxiety"),
          dep.var.labels=c("Willingness to Get Vaccinated"), 
          type = "text",
          out="firstregressions.txt")
#not a fan of this one

#Regression Models of interesting control variables:

model5 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS + setting_dummy,
             data = hps.clean)
summary(model5)
#setting isn't statistically significant at any level

model6 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS + rrace_dummy,
            data = hps.clean)
summary(model6)
#race dummy 2 is significant (Black) with a negative coefficient meaning black
#people are more likely to get vaccinated than not. Since their coefficient 
#decreases with an increase in GETVAC (which is backwards!!)
#race dummy 4 is also significant (any other race) with a positive coefficient

model7 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS + kindwork_dummy,
             data = hps.clean)
summary(model7)
#not statistically significant

model8 <- lm(GETVAC ~ hadcovid_dummy + INCOME + ANXIOUS + 
                     sexual_orientation_dummy,
                     data = hps.clean)
summary(model8)
#not statistically significant, except sexualorientation 4 (something else),
#with a positive coefficient of 10.877

#Graph of the rest of my regressions:

stargazer(model5, model6, model7, model8,
          type = "text",
          out = "interestregressions.html",
          title = "Summary of Regressions for Variables of Interest")

#Final Regression Model:
stargazer(model1, model2, model3, model6, title="Regression Results",
          digits = 2,
          covariate.labels = c("Hasn't had Covid", "Unsure If They Had Covid",
                               "Income", "Health Anxiety", "Black", "Asian", 
                               "Any Other Race"),
          dep.var.labels=c("Willingness to Get Vaccinated"), 
          type = "text",
          out="finalregressions.html")

#CHECKING OLS ASSUMPTIONS

#Normal Distribution:
plot(model6, 2, col = "light pink")
#has left skew

#Linearity:
plot(model6, 1, col = "pink")
#doesn't violate linearity assumption

#Multicolinearity:
vif(model6)
#all have small VIF's, thus low multicollinearity

#DFBeta:
dfbetasPlots(model6, col = "pink")
#only one mildly influential observation race 3 (asian)

#Heteroskedasticity:
bptest(model8)
#data is heteroskedastic :(
#To fix this -
correct_hetero <- coeftest(model8, vcov = vcovHC(model4, type = "HC1")) 
correct_hetero
#not having covid remains statistically insignificant, while having covid and
#not sure about having covid are significant
