# gen_cubic <- function (x)

x <- seq(-2,2,length.out=200)
y <- x^3  # solution x=0
plot(y~x)

x <- seq(-2,0,length.out=200)
y <- (x+1)^3  # solution x=-1
plot(y~x)

x <- seq(-2,0,length.out=200)
y <- (x+1)^3 + 1  # solution x=-1
plot(y~x)

x <- seq(-1,2,length.out=200)
y <- x^3 - 2*x^2  # solutions x=0, 2
plot(y~x)

x <- seq(-1,2,length.out=200)
y <- x^3 - 2*x^2 +0.5  # solutions x=0, 2
plot(y~x, type="l", col="blue", lwd=2)
abline(h=0, v=0)

y <- x^3 - 2*x^2 +0.5+rnorm(200)  # solutions x=0, 2
summary(model1 <- lm(y ~ I(x^3)+I(x^2)+I(x)))

plot(y~x)
abline(h=0, v=0)
lines(predict(model1)~x, col="blue", lwd=2)

x <- x - mean(x)

summary(model2 <- lm(y ~ I(x^3)+I(x^2)+I(x)))

plot(y~x)
abline(h=0, v=0)
lines(predict(model1)~x, col="blue", lwd=2)
