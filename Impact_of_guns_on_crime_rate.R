library(haven)
library(multcomp)
library(car)
library(ggplot2)
library(tidyverse)
library(fixest)
library(foreign)
library(lme4)
library(plm)


library(foreign)
library(GGally)
library("cowplot")

guns <- read_dta("guns.dta")
guns$stateid
#guns2 <- read_dta("guns.dta")
#write.csv(guns, file="guns.csv")

#df11 -> data excluding Washington DC (stateid = 11)


guns$shall <- as.factor(guns$shall)
guns$stateid <- as.factor(guns$stateid)
guns$crime_rate <- guns$vio+guns$mur+guns$rob
guns$crime_rate_ln <- log(guns$crime_rate)

df11 <- guns
df11exc <- subset(df11, !(df11$stateid %in% c(11)))


##df11 <- subset(guns, !(guns$stateid %in% c(11)))

hist(guns$vio)

#Data Summary
data.frame(mean = sapply(guns, mean, na.rm = TRUE)
           , median = sapply(guns, median, na.rm = TRUE)
           , min = sapply(guns, min, na.rm = TRUE)
           , max = sapply(guns, max, na.rm = TRUE)
           , sd = sapply(guns, sd, na.rm = TRUE)
           , miss.val = sapply(guns, function(x)
             sum(is.na(x))))

#Correlation
round(cor(guns),2)
round(cor(guns_non_DC),2)



hist(guns$vio)
boxplot(guns$vio~guns$year)
boxplot(guns$rob~guns$year)
boxplot(guns$mur~guns$year)
boxplot(guns$vio[guns$stateid==11]~guns$year[guns$stateid==11])
boxplot(guns$rob[guns$stateid==11]~guns$year[guns$stateid==11])
boxplot(guns$mur[guns$stateid==11]~guns$year[guns$stateid==11])
boxplot(guns$vio[guns$stateid!=11]~guns$year[guns$stateid!=11])
boxplot(guns$rob[guns$stateid!=11]~guns$year[guns$stateid!=11])
boxplot(guns$mur[guns$stateid!=11]~guns$year[guns$stateid!=11])


guns_subset_0 <- subset(guns, guns$stateid %in% c(1,6,8,9,10,11,15,17,19,20,24,25,26,27,29,31,34,35,36,39,44,55))
guns_subset_1 <- subset(guns, !(guns$stateid %in% c(1,6,8,9,10,11,15,17,19,20,24,25,26,27,29,31,34,35,36,39,44,55)))
guns_subset_0_no11 <- subset(guns, guns$stateid %in% c(1,6,8,9,10,15,17,19,20,24,25,26,27,29,31,34,35,36,39,44,55))


guns_subset_0_no11$stateid

round(cor(guns_subset_0),2)
round(cor(guns_subset_0_no11),2)

round(cor(guns_subset_1),2)


#states where gun laws were not passed 
boxplot(guns_subset_0$vio[guns_subset_0$stateid!=11]~guns_subset_0$year[guns_subset_0$stateid!=11])
boxplot(guns_subset_0$rob[guns_subset_0$stateid!=11]~guns_subset_0$year[guns_subset_0$stateid!=11])
boxplot(guns_subset_0$mur[guns_subset_0$stateid!=11]~guns_subset_0$year[guns_subset_0$stateid!=11])

#states where gun laws were passed
boxplot(guns_subset_1$vio~guns_subset_1$year)
boxplot(guns_subset_1$rob~guns_subset_1$year)
boxplot(guns_subset_1$mur~guns_subset_1$year)

#states where Shall laws were passed in 1990
boxplot(guns$vio[guns$stateid %in% c(16,28,41)]~guns$year[guns$stateid %in% c(16,28,41)])
boxplot(guns$rob[guns$stateid %in% c(16,28,41)]~guns$year[guns$stateid %in% c(16,28,41)])
boxplot(guns$mur[guns$stateid %in% c(16,28,41)]~guns$year[guns$stateid %in% c(16,28,41)])

#states where Shall laws were passed in 1990
boxplot(guns$vio[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])
boxplot(guns$rob[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])
boxplot(guns$mur[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])

#states where Shall laws were always present
boxplot(guns$vio[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])
boxplot(guns$rob[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])
boxplot(guns$mur[guns$stateid %in% c(13,42,54)]~guns$year[guns$stateid %in% c(13,42,54)])

boxplot(guns$vio~guns$stateid)
boxplot(guns$vio~guns$stateid)

#State ID =11, Crime rate is an outlier
boxplot(guns$shall[guns$stateid==11]~guns$year[guns$stateid==11])
plot(guns$vio[guns$stateid==11]~guns$year[guns$stateid==11])
plot(guns$year[guns$stateid==11]~guns$incarc_rate[guns$stateid==11])


#Stateid = 11 is an outlier

#Density and population
y <- 81

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$pop[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==80],guns$pop[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$pop[guns$stateid!=11])

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$density[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$density[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$density[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$density[guns$stateid!=11],1))


#pm1029 ,pw1064, and pb1064
y <- 81

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$pm1029[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$pm1029[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$pm1029[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$pm1029[guns$stateid!=11],1))

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$pw1064[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$pw1064[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$pw1064[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$pw1064[guns$stateid!=11],1))

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$pb1064[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$pb1064[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$pb1064[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$pb1064[guns$stateid!=11],1))


#Average Income
y <- 81

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$avginc[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$avginc[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$avginc[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$avginc[guns$stateid!=11],1))

#Incarceration Rate
y <- 81

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$incarc_rate[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11 & guns$year==y],guns$incarc_rate[guns$stateid!=11 & guns$year==y])
cor(guns$vio[guns$stateid!=11],guns$incarc_rate[guns$stateid!=11])
boxplot(guns$vio[guns$stateid!=11]~round(guns$incarc_rate[guns$stateid!=11],1))


#State
y <- 81

plot(guns$vio[guns$stateid!=11 & guns$year==y]~guns$stateid[guns$stateid!=11 & guns$year==y])

boxplot(guns$vio[guns$stateid!=11]~round(guns$stateid[guns$stateid!=11],1))


unique(guns$stateid[guns$vio < 100])


#Graph for Shall_0 vs Shall_1


guns_subset_0_summarized<-guns_subset_0 %>% group_by(year) %>% summarise(vio = sum(vio))
guns_subset_0_no11_summarized<-guns_subset_0_no11 %>% group_by(year) %>% summarise(vio = sum(vio))
guns_subset_1_summarized<-guns_subset_1 %>% group_by(year) %>% summarise(vio = sum(vio))

plot(guns_subset_0_summarized$vio~guns_subset_0_summarized$year, type='o', col = 'blue')

lines(guns_subset_1_summarized$vio~guns_subset_1_summarized$year, type='o', col = 'red')
lines(guns_subset_0_no11_summarized$vio~guns_subset_0_no11_summarized$year, type='o', col = 'black')

guns_<-guns %>% group_by(stateid) %>% summarise(vio = sum(vio), pb1064 = mean(pb1064), pm1029 = mean(pm1029)
                                                , pw1064 = mean(pw1064), avginc = mean(avginc)
                                                , density = mean(density), pop = mean(pop))
plot(guns_$vio~guns_$pb1064, col = 'black')
plot(guns_$vio~guns_$pm1029, col = 'blue')
plot(guns_$vio~guns_$pw1064, col = 'red')
plot(guns_$vio~guns_$avginc, col = 'green')
plot(guns_$vio~guns_$density, col = 'violet', xlim = c(0,1.3))
plot(guns_$vio~guns_$pop, col = 'purple')


cor(guns_)

#Data Modelling

#Linear Regressions (Non Log based) 
##Just for reference



model1 <- lm(crime_rate ~ year+ incarc_rate + pb1064 + pw1064 + pm1029 + pop
             + avginc + density + shall + stateid, data = df11)
summary(model1)

k <- plot(model1)

model1 <- lm(crime_rate ~ incarc_rate + pb1064 + pw1064 + pm1029 + pop
             + avginc + density + shall
             , data = df11)
summary(model1)

BIC(model1)


model1_2 <- lm(crime_rate ~ incarc_rate + I(incarc_rate^2) + pb1064 + pop + I(pop^2)
               + density + shall + avginc + I(avginc^2)
               , data = df11)
summary(model1_2)

BIC(model1_2)


model_pooled <- plm(crime_rate ~ incarc_rate + pb1064 + pw1064 + pm1029 + pop 
                    + avginc + density + shall
                    , data = df11, model = "pooling", index = c("stateid", "year"))
summary(model_pooled)



model_fe <- plm(crime_rate ~ incarc_rate + pb1064 + pm1029 + pop + pw1064
                + avginc + density + shall
                , data = df11, model = "within", index = c("stateid"))
summary(model_fe)


model_fe11 <- plm(crime_rate ~ incarc_rate + pb1064 + pm1029 + pop + pw1064
                  + avginc + density + shall
                  , data = df11exc, model = "within", index = c("stateid"))
summary(model_fe11)


model_fet <- plm(crime_rate ~ incarc_rate + pb1064 + pm1029 + pop + pw1064
                 + avginc + density + shall
                 , data = df11, model = "within", index = c("stateid", "year"), effect = "twoways")
summary(model_fet)


options(scipen=1)


##


#Pooled OLS

model_pooled_l <- plm(crime_rate_ln ~ incarc_rate + pb1064 + pw1064 + pm1029 + pop 
                      + avginc + density + shall
                      , data = df11, model = "pooling", index = c("stateid", "year"))
summary(model_pooled_l)


model_pooled_l_2 <- plm(crime_rate_ln ~ incarc_rate + pb1064 + pw1064 + pop 
                        + density + shall
                        , data = df11, model = "pooling", index = c("stateid", "year"))
summary(model_pooled_l_2)

model_pooled_l_3 <- plm(crime_rate_ln ~ incarc_rate + I(incarc_rate^2) + pb1064 + pw1064 + pop + I(pop^2) + I(pb1064^2)
                        + density + shall + avginc + I(avginc^2)
                        , data = df11, model = "pooling", index = c("stateid", "year"))
summary(model_pooled_l_3)




par(mfrow = c(1, 3))

plot(fitted(model_pooled_l) ~ df11$crime_rate_ln, xlab = "Observed values", ylab = "Fitted values", 
     main = "Regression Graph - 1st Model")
abline(lm(fitted(model_pooled_l) ~ df11$crime_rate_ln), col = "red")

plot(fitted(model_pooled_l_2) ~ df11$crime_rate_ln, xlab = "Observed values", ylab = "Fitted values", 
     main = "Regression Graph - 1st Model")
abline(lm(fitted(model_pooled_l_2) ~ df11$crime_rate_ln), col = "red")


plot(fitted(model_pooled_l_3) ~ df11$crime_rate_ln, xlab = "Observed values", ylab = "Fitted values", 
     main = "Regression Graph - 3rd Model")
abline(lm(fitted(model_pooled_l_3) ~ df11$crime_rate_ln), col = "red")






#Fixed Effects


model_fe_l <- plm(crime_rate_ln ~ incarc_rate + pb1064 + pm1029 + pop + pw1064
                  + avginc + density + shall
                  , data = df11, model = "within", index = c("stateid"))
summary(model_fe_l)
plot(model_fe_l)

model_fe_l_2 <- plm(crime_rate_ln ~ incarc_rate + I(incarc_rate^2) + pb1064 + pw1064 + pm1029 + pop + I(pop^2) + I(pb1064^2)
                  + density + shall + avginc + I(avginc^2)+ I(pb1064^2)
                  , data = df11, model = "within", index = c("stateid"))
summary(model_fe_l_2)

model_fe_l_3 <- plm(crime_rate_ln ~ pb1064 + pw1064 + pm1029 + pop + I(pop^2)
                    + density + shall + avginc + I(avginc^2)
                    , data = df11, model = "within", index = c("stateid"))
summary(model_fe_l_3)




#Fixed Time Effects

model_fet_l <- plm(crime_rate_ln ~ incarc_rate + pb1064 + pm1029 + pop + pw1064
                   + avginc + density + shall
                   , data = df11, model = "within", index = c("stateid", "year"), effect = "twoways")
summary(model_fet_l)


model_fet_l_2 <- plm(crime_rate_ln ~ pb1064 + pm1029 + pop + I(pop^2) 
                     + density + shall + avginc + I(avginc^2)
                   , data = df11, model = "within", index = c("stateid", "year"), effect = "twoways")
summary(model_fet_l_2)


model_fe_l_2_test <- plm(crime_rate_ln ~ pb1064 + pm1029 + pop + I(pop^2) 
                     + density + shall + avginc + I(avginc^2)
                     , data = df11, model = "within", index = c("stateid"))
summary(model_fe_l_2_test)

pFtest(model_fet_l_2,model_fe_l_2_test)

