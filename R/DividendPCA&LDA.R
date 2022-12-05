ARTofR::xxx_title2('4630 Project: Analysis of Dividend Data-set')
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                4630 Project: Analysis of Dividend Data-set               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries ---------------------------------------------------------------
library(psych)
library(devtools)
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
install_github("vqv/ggbiplot")
library(ggbiplot)
library(MASS)
library(pROC)
#library(ggplot2)


# Data set ----------------------------------------------------------------

df=read.csv(file.choose(), header = T)
df$dividend = factor(df$dividend)
str(df)
View(df)

# Understanding the Data --------------------------------------------------

# Our independent variables are as follows:

##### fcfps: Free cash flow per share (in $) 

# Definition: A measure of a company's financial flexibility that is determined 
# by dividing free cash flow by the total number of shares outstanding. This 
# measure serves as a proxy for measuring changes in earnings per share. 
# Ideally a business will generate more cash flow than is required for operational
# expenses and capital expenditures. If they do, the metric below will increase,
# as the numerator grows holding shares outstanding constant. It is a positive sign
# for an increase in free cash flow to outstanding shares value, and is viewed that 
# the company is regarded as improving prospects and more financial & operational
# flexibility

##### earnings_growth: Earnings growth: in the past year (in %)

# Definition: The change in an entity's report net income over a period of time.
# The measure is usually a period-to-period comparison. The concept can also be
# used to estimate growth in a future period over the current period. A high 
# level of earnings growth is more likely to drive up the market price of a 
# company's stock. Conversely, if the earnings growth rate declines, this can 
# trigger a sell off by investors that drive down the stock price. 

###### de: Debt to Equity ratio

# Definition: This is used to evaluate a company's financial leverage and is 
# calculated by dividing a company's total liabilities by its shareholder equity.
# This is an important metric in corporate finance. It is a measure of the degree 
# to which a company is financing its operations with debt rather than its own 
# resources.D/E varies by industry and are best used to compare direct competitors.
# A higher D/E ratio suggests more risk, while low may indicate the business is 
# not taking advantage of debt financing to expand. 


###### mcap: Market Capitalization of the stock 

# Definition: Refers to the total dollar market value of a company's outstanding
# shares of stock. The investment community uses this figure to determine a 
# company's size instead of sales or total asset figures. In an acquisition, 
# the market cap is used to determine whether a takeover candidate represents a 
# good value or not to the acquirer.



##### current_ratio: Current Ratio (or Current Assets/Current Liabilities)

# Definition: A Liquidity ratio that measures a company's ability to pay short 
# term obligations or those due within one year. It tells investors and analyst
# how a company can maximize the current assets on its balance sheet to satisfy
# its current debt and other payables. 

# A current ratio that is in line with the industry average or slightly higher is
# generally considered acceptable. If lower, then it indicates a higher risk of
# distress or default. If it is very high, it indicates management may not be 
# using its assets efficiently.


#Dependent Variable
#dividend: 1 if a stock pays dividend

#Want to figure out if a stock pays dividend or not. 


# PC Analysis-----------------------------------------------------------
pairs.panels(df[,-1], gap = 0, 
             bg = c("red", "blue")[df$dividend], pch = 21 )

#Based off the plot, we may see a slightly higher level of correlation between 
#current_ratio and mcap. PCA may aid in dealing with this situation. 


#PCA Analysis 
#We first need to scale the ind. variables, as they each are measured quite 
#differently.
pc = prcomp(df[,-1], center = TRUE, scale. = TRUE)
attributes(pc)
pc
summary(pc)

pairs.panels(pc$x, gap = 0, 
             bg = c("red", "blue")[t=df$dividend], pch = 21)


#Bi-Plot
g <- ggbiplot(pc, choices = 3:4,
              obs.scale = 1,
              var.scale = 1,
              groups = df$dividend,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

#Scree Plot
var_exp = pc$sdev^2/sum(pc$sdev^2)
var_exp


plot(var_exp, type = 'b', col = 'green', yaxt = 'n', ylab = '', xlab = '')
par(new = T)
plot(cumsum(var_exp), xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     main = "Scree Plot Using Correlation Matrix",
     ylim = c(0, 1), type = "b", col = 'blue')
legend(4.2, 0.4, legend=c("Proportion", "Cumulative"),
       col=c("green", "blue"), lty=1:1, cex=0.8)

# Seems like 4th PC components will be useful for our data.


# New Data set with PC ----------------------------------------------------
df_pc = pc$x[,1:4]
df_pc = cbind(df_pc, df$dividend)
colnames(df_pc)[5] = "dividend"
df_pc = as.data.frame(df_pc)
df_pc$dividend = factor(df_pc$dividend)


# LDA ---------------------------------------------------------------------
####### Linear discriminant model without USING the PC
model = lda(dividend ~., data = df)
model

# Let us check the prediction accuracy when using LDA
set.seed(1000)
sample = sample(c(TRUE,FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train = df[sample, ]
test = df[!sample, ]

model_train = lda(dividend ~., data = train)

# using model to make predictions
predicted = predict(model_train, test)

# accuracy of model
acc = mean(predicted$class==test$dividend)
round(acc, 4)

#Accuracy indicates 97.3%


# Cross Validation 5 times where 80% is used as the training data.
set.seed(3)
rep = 1000
errlin = dim(rep)
for (i in 1: rep){
  training = sample(1:200, 163)
  trg = df[training,]
  tst = df[-training,]
  mod = lda(dividend ~., data = trg)
  pred = predict(mod, tst)$class
  tablin = table(tst$dividend, pred)
  errlin[i] = (dim(tst)[1] - sum(diag(tablin)))/dim(tst)[1]
}
merrlin = mean(errlin)
merrlin

#We have a miss-classification rate of 6.5% when doing cross validation a 1000 
#times

# define data to plot
lda_plot = cbind(train, predict(model_train)$x)
# double-color scatter plot
lda.values = predict(model_train, train)
class = predict(model_train)$class
# blue is class = 1, green is class = 0
plot(lda.values$x[,1], type="p", xlim=c(0,30), ylab=c("LDA Axis 1"),
     col=c(as.numeric(class)+10))
abline(h = 0)








####### Linear discriminant model Using the PC
#Linear discriminant model
model = lda( dividend ~ PC1 + PC2 + PC3 + PC4, data = df_pc)
model

# Let us check the prediction accuracy when using LDA
set.seed(1000)
sample = sample(c(TRUE,FALSE), nrow(df_pc), replace=TRUE, prob=c(0.8,0.2))
train = df_pc[sample, ]
test = df_pc[!sample, ]

model_train = lda(dividend ~., data = train)

# using model to make predictions
predicted = predict(model_train, test)

# accuracy of model
acc = mean(predicted$class==test$dividend)
round(acc, 4)

#Accuracy indicates 97.3% which is the same as when we did not do PCA.


# Cross Validation 5 times where 80% is used as the training data
set.seed(2)
rep = 1000
errlin = dim(rep)
for (i in 1: rep){
  training = sample(1:200, 163)
  trg = df_pc[training,]
  tst = df_pc[-training,]
  mod = lda(dividend ~ PC1 + PC2 + PC3 + PC4, data = trg)
  pred = predict(mod, tst)$class
  tablin = table(tst$dividend, pred)
  errlin[i] = (dim(tst)[1] - sum(diag(tablin)))/dim(tst)[1]
}
merrlin = mean(errlin)
merrlin

#We have a miss-classification rate of 6.57% which is almost the exact same
#as what we had when we did do PCA. 







