trans <- matrix(runif(4), ncol=2) # arbitrary linear transformation

m1 <- lm(mpg ~ wt, data=mtcars)
mpg1 <- predict(m1)

m2 <- lm(mpg ~ 0 + model.matrix(m1) %*% trans, data=mtcars)
mpg2 <- predict(m2)

plot(mpg1 ~ mpg2)

norm(as.matrix(mpg1-mpg2), "F")

cbind(coef(m1), coef(m2))
trans %*% coef(m2)
solve(trans) %*% coef(m1)

mcenter <- matrix(c(1,0,-mean(mtcars$wt),1), ncol=2)

colMeans(model.matrix(m1) %*% mcenter)
