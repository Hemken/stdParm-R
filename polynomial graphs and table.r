reset.data <- function() {
  mtcars <- datasets::mtcars
  mtcars <- mtcars[order(mtcars$wt),]
  mtcars
}

mtcars <- reset.data()

onevar <- lm(mpg~wt, data=mtcars)
mtcars$yhat <- predict(onevar)

plot(mpg ~ wt, data=mtcars, main="Original Units")
points(yhat ~ wt, data=mtcars, type="l", col="red")

mtcars$wt <- mtcars$wt - mean(mtcars$wt)
mtcars$mpg <- mtcars$mpg - mean(mtcars$mpg)

centered <- lm(mpg~wt, data=mtcars)
mtcars$yhat <- predict(centered)

plot(mpg ~ wt, data=mtcars, main="Centered", xlab="wt - mean(wt)")
points(yhat ~ wt, data=mtcars, type="l", col="red")

mtcars$wt <- mtcars$wt/sd(mtcars$wt)
mtcars$mpg <- mtcars$mpg/sd(mtcars$mpg)

stdized <- lm(mpg~wt, data=mtcars)
mtcars$yhat <- predict(stdized)

plot(mpg ~ wt, data=mtcars, main="Standardized", xlab="(wt - mean(wt))/sd(wt)")
points(yhat ~ wt, data=mtcars, type="l", col="red")

coeftbl <- cbind(coef(onevar),coef(centered),coef(stdized))
colnames(coeftbl) <- c("Original", "Centered", "Standardized")
zapsmall(coeftbl, digits=3)

###############
mtcars <- reset.data()

polymodel <- (lm(mpg~(wt+I(wt^2)) + vs, data=mtcars))
summary(polymodel)
mtcars$yhat <- predict(polymodel)

plot(mpg ~ wt, data=mtcars, main="Original Units")
points(yhat ~ wt, data=mtcars[mtcars$vs==0,], type="l", col="red")
points(yhat ~ wt, data=mtcars[mtcars$vs==1,], type="l", col="red")

mtcars$wt <- mtcars$wt - mean(mtcars$wt)
mtcars$mpg <- mtcars$mpg - mean(mtcars$mpg)
centeredmodel <- (lm(mpg~(wt+I(wt^2)) + vs, data=mtcars))
mtcars$yhat <- predict(centeredmodel)

plot(mpg ~ wt, data=mtcars, main="Centered Units", 
     xlab=expression(wt[delta]), ylab=expression(mpg[delta]))
points(yhat ~ wt, data=mtcars[mtcars$vs==0,], type="l", col="red")
points(yhat ~ wt, data=mtcars[mtcars$vs==1,], type="l", col="red")

mtcars$wt <- mtcars$wt/sd(mtcars$wt)
mtcars$mpg <- mtcars$mpg/sd(mtcars$mpg)
stdmodel <- (lm(mpg~(wt+I(wt^2)) + vs, data=mtcars))
mtcars$yhat <- predict(stdmodel)

plot(mpg ~ wt, data=mtcars, main="Standardized Units",
     xlab=expression(wt[sigma]),
     ylab=expression(mpg[sigma]))
points(yhat ~ wt, data=mtcars[mtcars$vs==0,], type="l", col="red")
points(yhat ~ wt, data=mtcars[mtcars$vs==1,], type="l", col="red")

coeftblp <- cbind(coef(polymodel),coef(centeredmodel),coef(stdmodel))
colnames(coeftblp) <- c("Original", "Centered", "Standardized")
zapsmall(coeftblp, digits=3)

mtcars <- reset.data()