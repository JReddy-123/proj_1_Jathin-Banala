# Load & display dataset
load("C:/Users/manoh/Documents/Data_sets/R/Exercises&Examples/CLERICAL.Rdata")
CLERICAL


#Declare variables
DAY = CLERICAL$DAY
# OBS = CLERICAL$OBS
x1 = CLERICAL$X1 # Number of pieces of mail processed
x2 = CLERICAL$X2 # Number of money orders & gift certificates sold
x3 = CLERICAL$X3 # Number of window payments transacted
x4 = CLERICAL$X4 # Number of change order transactions processed
x5 = CLERICAL$X5 # Number of checks cashed
x6 = CLERICAL$X6 # Num. of pieces of misc. mail processed on an "as available" basis
x7 = CLERICAL$X7 # Number of bus tickets sold
y = CLERICAL$Y   # Number of hours worked per day by clerical staff

fit <- lm(y ~ x1+x2+x3+x4+x5+x6+x7)
summary(fit)
anova(fit) # Generates an Analysis of Variances Table


res = fit$residuals
res
win.graph()
plot(res)
stand.res = res/sd(res)   # Standardized residuals
Yhat = fit$fitted.values


library(MASS)
st.res = studres(fit)   # Studentized residuals

# Prroduces and displays residual plots
win.graph()
par(mfrow=c(1,2))
plot(Yhat, stand.res, main="Standardized Residuals vs Fitted Values", xlab="Fitted Values", ylab="Standardized Residuals")
plot(Yhat, st.res, main="Studentized Residuals vs Fitted Values", xlab="Fitted Values", ylab="Studentized Residuals")

win.graph()
qqnorm(st.res, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores", 
       main="Normal Probability Plot of Standardized Residuals") 
qqline(st.res) # by default: normal