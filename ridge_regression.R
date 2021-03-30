#[Ridge regression]#

install.packages("MASS")  #install packages to ridge
install.packages("glmnet")
library(MASS)
library(glmnet)

#ridge regression for Boston data

x=model.matrix(medv~.,Boston)[,-1] #x, y
y=Boston$medv

grid<-10^seq(10,-2,length=100) 
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

ridge.mod$lambd[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

ridge.mod$lambda[60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:14,]

#train, test
set.seed(1)
train<-sample(1:nrow(x),nrow(x)/2)
test<-(-train)
y.test<-y[test]
ridge.mod<-glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred<-predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

ridge.pred<-predict(ridge.mod,s=1e+10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred<-predict(ridge.mod,s=0,newx=x[test,])
mean((ridge.pred-y.test)^2)

lm(y~x,subset=train)

predict(ridge.mod,s=0,type="coefficients")[1:14,]
set.seed(1)
cv.out<-cv.glmnet(x[train,],y[train],alpha=0)
bestlam<-cv.out$lambda.min
bestlam

ridge.pred<-predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

out<-glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:14,]
