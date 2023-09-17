# ========
# scatter plot for all the veriable 
# ========
library("readxl")
setwd("C:/Users/hojk8/OneDrive/Desktop/Project I/data/")
data <- read_excel("2. file_want_column.xlsx")
plot(data, main = "Scatter Plot for All Variable")

# ========
# check is all the data inside the final clean file are all related to target variable 
# ========
setwd("C:/Users/hojk8/OneDrive/Desktop/Project I/data/Manually/")
FeatureTranformingWoe<- read_excel("7. file_FeatureTranformingWoe.xlsx")
names(FeatureTranformingWoe)

model <- lm(FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS" + FeatureTranformingWoe$"AGE" + FeatureTranformingWoe$"REPAYMENT_STATUS_IN_SEPTEMBER" + FeatureTranformingWoe$"AMOUNT_OF_BILL_STATEMENT_IN_SEPTEMBER" + FeatureTranformingWoe$"AMOUNT_OF_PREVIOUS_PAYMENT_IN_SEPTEMBER")

summary(model)

confint(model, level=.95)
anova(model)

# =======
# table of R square, Adjusted R square
# =======
model.fit1 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT")
sum.fit1 <- summary(model.fit1)

model.fit2 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX")
sum.fit2 <- summary(model.fit2)

model.fit3 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS")
sum.fit3 <- summary(model.fit3)

model.fit4 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS" + FeatureTranformingWoe$"AGE")
sum.fit4 <- summary(model.fit4)

model.fit5 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS" + FeatureTranformingWoe$"AGE" + FeatureTranformingWoe$"REPAYMENT_STATUS_IN_SEPTEMBER")
sum.fit5 <- summary(model.fit5)

model.fit6 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS" + FeatureTranformingWoe$"AGE" + FeatureTranformingWoe$"REPAYMENT_STATUS_IN_SEPTEMBER" + FeatureTranformingWoe$"AMOUNT_OF_BILL_STATEMENT_IN_SEPTEMBER")
sum.fit6 <- summary(model.fit6)

model.fit7 <- lm(formula = FeatureTranformingWoe$"default payment next month" ~ FeatureTranformingWoe$"AMOUNT_OF_GIVEN_CREDIT" + FeatureTranformingWoe$"SEX" + FeatureTranformingWoe$"MARITAL_STATUS" + FeatureTranformingWoe$"AGE" + FeatureTranformingWoe$"REPAYMENT_STATUS_IN_SEPTEMBER"+ FeatureTranformingWoe$"AMOUNT_OF_BILL_STATEMENT_IN_SEPTEMBER" + FeatureTranformingWoe$"AMOUNT_OF_PREVIOUS_PAYMENT_IN_SEPTEMBER")
sum.fit7 <- summary(model.fit7) 

R.sq.values <- data.frame(Model = c("AMOUNT_OF_GIVEN_CREDIT", "AMOUNT_OF_GIVEN_CREDIT, SEX", "AMOUNT_OF_GIVEN_CREDIT, SEX, MARITAL_STATUS", "AMOUNT_OF_GIVEN_CREDIT, SEX, MARITAL_STATUS, AGE", "AMOUNT_OF_GIVEN_CREDIT, SEX, MARITAL_STATUS, AGE, REPAYMENT_STATUS_IN_SEPTEMBER", "AMOUNT_OF_GIVEN_CREDIT, SEX, MARITAL_STATUS, AGE, REPAYMENT_STATUS_IN_SEPTEMBER, AMOUNT_OF_BILL_STATEMENT_IN_SEPTEMBER", "AMOUNT_OF_GIVEN_CREDIT, SEX, MARITAL_STATUS, AGE, REPAYMENT_STATUS_IN_SEPTEMBER, AMOUNT_OF_BILL_STATEMENT_IN_SEPTEMBER, AMOUNT_OF_PREVIOUS_PAYMENT_IN_SEPTEMBER"), 
                          R.Square = c(sum.fit1$r.squared, sum.fit2$r.squared, sum.fit3$r.squared, sum.fit4$r.squared, sum.fit5$r.squared, sum.fit6$r.squared, sum.fit7$r.squared),
                          Adjusted.R.Square = c(sum.fit1$adj.r.squared, sum.fit2$adj.r.squared, sum.fit3$adj.r.squared, sum.fit4$adj.r.squared, sum.fit5$adj.r.squared, sum.fit6$adj.r.squared, sum.fit7$adj.r.squared))
R.sq.values

# ======
# Diagnostic for Leverage and Influence 
# ======
# fitted values, y hat
yhat <- round(model$fitted.values,4)

# residuals, ei
ei <- round(model$residuals,4)

# stardized residuals, di
di <- round(model$residuals/sum.fit$sigma,4)


# studentized residuals, ri
ri <- round(rstandard(model),4)

# PRESS residuals, e(i)
hii <- round(hatvalues(model),4)
press <- round(ei/(1-hii),4)

# R-student residuals, ti
ti <- round(rstudent(model),4)

diagnostic_table <- cbind(yhat, ei, di, hii, ri, press, ti)
diagnostic_df <- as.data.frame.matrix(diagnostic_table)
diagnostic_df

# put this table into excel file 
library("writexl")
# write_xlsx(diagnostic_df,"C:/Users/hojk8/OneDrive/Desktop/Project II/dataset/Manually/10. diagnostic.xlsx")

n <- length(model$fitted.values)
cv1 <- qt(p=1-0.05/(2*n), df=model$df.residual-1)
cv1
cv2 <- qt(p=1-0.05/(2*n), df=model$df.residual)
cv2

# plot ri vs yhat
plot(x=model$fitted.values, y=ri, xlab="Estimated Mean Square", ylab="Studentized Residuals", main="r[i] vs estimated mean response"
     , panel.first = grid(col="gray", lty="dotted"), ylim=c(min(qt(p=0.10/(2*n), df=model$df.residual), min(ri)), max(qt(p=1-0.10/(2*n)
     , df=model$df.residual), max(ri))))
abline(h=0, col="darkgreen")
abline(h=c(qt(p=0.05/(2*n), df=model$df.residual), qt(p=1-0.05/(2*n), df=model$df.residual)), col="darkred", lwd=2)

# plot ti vs yhat
plot(x=model$fitted.values, y=ti, xlab="Estimated Mean Square", ylab="R-Student Residuals", main="t[i] vs estimated mean response"
     , panel.first = grid(col="gray", lty="dotted"), ylim=c(min(qt(p=0.10/(2*n), df=model$df.residual-1), min(ti)), max(qt(p=1-0.10/(2*n)
     , df=model$df.residual-1), max(ti))))
abline(h=0, col="darkgreen")
abline(h=c(qt(p=0.05/(2*n), df=model$df.residual-1), qt(p=1-0.05/(2*n), df=model$df.residual-1)), col="darkred", lwd=2)

# =====
# Influential Cases 
# =====
dffits.i <- round(dffits(model),4)
cook.i <- round(cooks.distance(model),4)
dfbeta.all <- round(dfbetas(model),4)

influential_table <- cbind(dffits.i, cook.i, dfbeta.all)
influential_df <- as.data.frame.matrix(influential_table)
influential_df

# put this table into excel file 
library("writexl")
#write_xlsx(influential_df,"C:/Users/hojk8/OneDrive/Desktop/Project II/dataset/Manually/11. influential.xlsx")

# critical value for cook's distance 
# little influence 
# major influence 
cook_little <- qf(p=0.2, df1=8, df2=24429, lower.tail=FALSE)
cook_little
cook_major <- qf(p=0.5, df1=8, df2=24429, lower.tail=FALSE)
cook_major

2/sqrt(24437)
















