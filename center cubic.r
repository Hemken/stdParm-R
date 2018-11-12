set.seed(20181111)
x <- seq(-1,2,length.out=200)
y <- x^3 - 2*x^2 +0.5 + rnorm(200)  # solutions x=0, 2
summary(model1 <- lm(y ~ x + I(x^2) + I(x^3)))

plot(y~x, main="Original")
abline(h=0, v=0)
lines(predict(model1)~x, col="blue", lwd=2)

summary(model2 <- lm(y ~ poly(x,3)))
px <- poly(x,3)
plot(y~px[,1], main="Orthonormal 1")
abline(h=0, v=0)
lines(predict(model2)~px[,1], col="blue", lwd=2)

# plot(y~px[,2], main="Orthonormal 2")
# abline(h=0, v=0)
# lines(predict(model2)~px[,2], col="blue", lwd=2)
# 
# plot(y~px[,3], main="Orthonormal 3")
# abline(h=0, v=0)
# lines(predict(model2)~px[,3], col="blue", lwd=2)


x <- x - mean(x)

summary(model3 <- lm(y ~ x + I(x^2) + I(x^3)))

plot(y~x, main="Centered")
abline(h=0, v=0)
lines(predict(model3)~x, col="blue", lwd=2)

cbind(coef(model1),coef(model2), coef(model3))

source("stdParm functions.r")
x <- seq(-1,2,length.out=200)
mu <- mean(x)
names(mu) <- "x"
A <- mean.to.matrix(mu) # recentering x - 3
C <- kron(A,A)
C <- kron(A,C)
C
# clean extra columns
b <- coef(model1)
names(b)[3:4] <- c("x:x", "x:x:x") 
cnames <- colnames(C)
keepcols <- match(names(b), colnames(C))
C <- C[, keepcols]
colnames(C) <- cnames[keepcols]
C

cnames <- colnames(C)
rnames <- rownames(C)
rowcombine <- matrix(NA, nrow=length(cnames), ncol=length(rnames))
for (i in 1:length(cnames)) {
  rowcombine[i,] <- rnames %in% cnames[i]
}
C <- rowcombine %*% C
rownames(C) <- cnames
C

C%*%b
coef(model3)

y1 <- predict(model1)
y2 <- predict(model2)
y3 <- predict(model3)

d <- x-mean(x)
y4 <- as.matrix(data.frame(1,d,d^2,d^3))%*%(C%*%b)
sum(y1==y2); sqrt(sum(y1-y2)^2)
sum(y1==y3); sqrt(sum(y1-y3)^2)
sum(y1==y4); sqrt(sum(y1-y4)^2)

plot(data.frame(y1,y2,y3,y4))
