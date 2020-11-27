########################Multi Linear Regression#####################################

#Problem Statement:Consider only the below columns and prepare a prediction model for predicting Price.

#Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

# Data : ToyotaCorolla.csv
#####################################################################################
install.packages("psych")
library(psych)
# Diagnostic Plots
install.packages("car")
library(car)

toyotaDetails <- read.csv(file.choose()) #ToyotaCorolla.csv

toyotaDetails<-toyotaDetils[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
attach(toyotaDetails)
View(toyotaDetails)
summary(toyotaDetails)

#Price         Age_08_04           KM               HP              cc            Doors      
#Min.   : 4350   Min.   : 1.00   Min.   :     1   Min.   : 69.0   Min.   : 1300   Min.   :2.000  
#1st Qu.: 8450   1st Qu.:44.00   1st Qu.: 43000   1st Qu.: 90.0   1st Qu.: 1400   1st Qu.:3.000  
#Median : 9900   Median :61.00   Median : 63390   Median :110.0   Median : 1600   Median :4.000  
#Mean   :10731   Mean   :55.95   Mean   : 68533   Mean   :101.5   Mean   : 1577   Mean   :4.033  
#3rd Qu.:11950   3rd Qu.:70.00   3rd Qu.: 87021   3rd Qu.:110.0   3rd Qu.: 1600   3rd Qu.:5.000  
#Max.   :32500   Max.   :80.00   Max.   :243000   Max.   :192.0   Max.   :16000   Max.   :5.000  


#Gears       Quarterly_Tax        Weight    
#Min.   :3.000   Min.   : 19.00   Min.   :1000  
#1st Qu.:5.000   1st Qu.: 69.00   1st Qu.:1040  
#Median :5.000   Median : 85.00   Median :1070  
#Mean   :5.026   Mean   : 87.12   Mean   :1072  
#3rd Qu.:5.000   3rd Qu.: 85.00   3rd Qu.:1085  
#Max.   :6.000   Max.   :283.00   Max.   :1615  

#Scatter plot for all pairs of variables. 
pairs(toyotaDetails)

# correlation matrix
cor(toyotaDetails)

#                   Price    Age_08_04          KM          HP          cc       Doors        Gears
#Price          1.00000000 -0.876590497 -0.56996016  0.31498983  0.12638920  0.18532555  0.063103857
#Age_08_04     -0.87659050  1.000000000  0.50567218 -0.15662202 -0.09808374 -0.14835921 -0.005363947
#KM            -0.56996016  0.505672180  1.00000000 -0.33353795  0.10268289 -0.03619661  0.015023328
#HP             0.31498983 -0.156622020 -0.33353795  1.00000000  0.03585580  0.09242450  0.209477146
#cc             0.12638920 -0.098083739  0.10268289  0.03585580  1.00000000  0.07990330  0.014629352
#Doors          0.18532555 -0.148359215 -0.03619661  0.09242450  0.07990330  1.00000000 -0.160141430
#Gears          0.06310386 -0.005363947  0.01502333  0.20947715  0.01462935 -0.16014143  1.000000000
#Quarterly_Tax  0.21919691 -0.198430508  0.27816470 -0.29843172  0.30699580  0.10936323 -0.005451955
#Weight         0.58119759 -0.470253184 -0.02859846  0.08961406  0.33563740  0.30261764  0.020613284

#              Quarterly_Tax      Weight
#Price           0.219196911  0.58119759
#Age_08_04      -0.198430508 -0.47025318
#KM              0.278164697 -0.02859846
#HP             -0.298431717  0.08961406
#cc              0.306995798  0.33563740
#Doors           0.109363225  0.30261764
#Gears          -0.005451955  0.02061328
#Quarterly_Tax   1.000000000  0.62613373
#Weight          0.626133733  1.00000000

#Building multi linear regression model
model <- lm(Price ~., data= toyotaDetails)
summary(model)

#Call:
#  lm(formula = Price ~ ., data = toyotaDetails)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9366.4  -793.3   -21.3   799.7  6444.0 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -5.573e+03  1.411e+03  -3.949 8.24e-05 ***
#  Age_08_04     -1.217e+02  2.616e+00 -46.512  < 2e-16 ***
#  KM            -2.082e-02  1.252e-03 -16.622  < 2e-16 ***
#  HP             3.168e+01  2.818e+00  11.241  < 2e-16 ***
#  cc            -1.211e-01  9.009e-02  -1.344  0.17909    
#Doors         -1.617e+00  4.001e+01  -0.040  0.96777    
#Gears          5.943e+02  1.971e+02   3.016  0.00261 ** 
#  Quarterly_Tax  3.949e+00  1.310e+00   3.015  0.00262 ** 
#  Weight         1.696e+01  1.068e+00  15.880  < 2e-16 ***
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1342 on 1427 degrees of freedom
#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863 
#F-statistic:  1131 on 8 and 1427 DF,  p-value: < 2.2e-16

#Multiple R-squared:  0.8638,	Adjusted R-squared:  0.863, cc and doors are influence to each other.
#Checking signifance by building individual models respectively.

##Model building using cc variable.
ccmodel <- lm(Price ~ cc, data=toyotaDetails)
summary(ccmodel)

#Call:
#  lm(formula = Price ~ cc, data = toyotaDetails)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7360.2 -2305.8  -855.8  1194.2 21312.1 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 9027.5548   365.5755  24.694  < 2e-16 ***
#  cc             1.0802     0.2239   4.825 1.55e-06 ***
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3599 on 1434 degrees of freedom
#Multiple R-squared:  0.01597,	Adjusted R-squared:  0.01529 
#F-statistic: 23.28 on 1 and 1434 DF,  p-value: 1.551e-06
##cc variable is significant in building model.

##Model building using Doors variable

doorsModel <- lm(Price ~ Doors, data= toyotaDetails)
summary(doorsModel)
#Call:
#  lm(formula = Price ~ Doors, data = toyotaDetails)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7062.8 -2251.7  -915.3   958.0 21087.2 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  7885.01     409.44  19.258  < 2e-16 ***
#  Doors         705.56      98.79   7.142 1.46e-12 ***
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 3565 on 1434 degrees of freedom
#Multiple R-squared:  0.03435,	Adjusted R-squared:  0.03367 
#F-statistic:    51 on 1 and 1434 DF,  p-value: 1.461e-12

## Doors variable is also significant.

##Building model using both doors and cc
carModel <- lm(Price~cc+Doors,data = toyotaDetails)
summary(carModel)

#Call:
#  lm(formula = Price ~ cc + Doors, data = toyotaDetails)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7243.9 -2273.6  -821.3  1054.4 20714.1 
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 6509.4211   515.7732  12.621  < 2e-16 ***
#  cc             0.9597     0.2211   4.340 1.52e-05 ***
#  Doors        671.3973    98.5009   6.816 1.37e-11 ***
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 3543 on 1433 degrees of freedom
#Multiple R-squared:  0.04688,	Adjusted R-squared:  0.04555 
#F-statistic: 35.24 on 2 and 1433 DF,  p-value: 1.15e-15

##Both the varibles are significant when used together in buiding model.

##Finding out the influence record.

# Deletion Diagnostics for identifying influential variable
influence.measures(model)
influenceIndexPlot(model) # Index Plots of the influence measures
influencePlot(model)# A user friendly representation of the above
?influencePlot

#StudRes       Hat      CookD
#81   8.164500 0.9182368 79.5201062
#222 -7.673262 0.1397116  1.0210312
#961 -5.456195 0.1572484  0.6049996

#Deleting influentail record 81 and building the model
model.car1 <- lm(Price~.,data= toyotaDetails[-81,])
summary(model.car1)


