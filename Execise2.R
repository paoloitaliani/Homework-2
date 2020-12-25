setwd("~/Desktop/Università/MODERN STATISTICS AND BIG DATA ANALYTICS ")
###EXERCISE 1 
#######a)

library(pdfCluster)
data(oliveoil)
olive= oliveoil[,3:10]
vow=read.table("vowels.dat.txt",header = T)
artdat=read.table("clusterdata2.txt")

diag(var(vow))
diag(var(artdat))
diag(var(olive))
solive=scale(olive)

#olive
library(cluster)
set.seed(123456)
cg1.olive <- clusGap(solive,kmeans,K.max = 10,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)

plot(cg1.olive,main="")

gl<-print(cg1.olive,method="globalSEmax",SE.factor=2)

tib<-print(cg1.olive,method="Tibs2001SEmax",SE.factor=3)

#vowels
library(cluster)
set.seed(123456)
cg1.vow <- clusGap(vow,kmeans,K.max = 10,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)



plot(cg1.vow,main="")

print(cg1.vow,method="globalSEmax",SE.factor=2)
print(cg1.vow,method="Tibs2001SEmax",SE.factor=2)



#clusterdata

set.seed(123456)
cg1.art <- clusGap(artdat,kmeans,K.max = 10,B=100,d.power=2,spaceH0="scaledPCA",nstart=100)

plot(cg1.art,main="")

plot(1:10,exp(cg1.art$Tab[,1]),xlab="k",ylab="S_k",type="l")
points(1:10,exp(cg1.art$Tab[,2]),xlab="k",ylab=" S_k",type="l",lty=2)
legend(3,20000,c(" S_k in data","E( S_k) uniform"),lty=1:200)

print(cg1.art,method="globalSEmax",SE.factor=2)
print(cg1.art,method="Tibs2001SEmax",SE.factor=2)


######)b
set.seed(123)
U.1<-runif(n=140, min=min(artdat$V1), max=max(artdat$V1))
U.2<-runif(n=140, min=min(artdat$X1.15561482046003), max=max(artdat$X1.15561482046003))

U<-data.frame(U.1,U.2)
set.seed(123)
U.tw <- sapply(1:10, 
              function(k){kmeans(U, k, nstart=100,iter.max = 15 )$tot.withinss})
set.seed(123)
U.clus=sapply(1:10, 
              function(k){kmeans(U, k, nstart=50,iter.max = 15 )$cluster})

art.tw=sapply(1:10, 
             function(k){kmeans(artdat, k, nstart=100,iter.max = 15 )$tot.withinss})

plot(U.1,U.2,col=U.clus[,2])
plot(U.1,U.2,col=U.clus[,3])
plot(U.1,U.2,col=U.clus[,5])
plot(U.1,U.2,col=U.clus[,8])


plot(1:10,log(art.tw),xlab="k",ylab="log S_k",type="l")
points(1:10,log(U.tw),xlab="k",ylab=" S_k",type="l",lty=2)

legend(1.7,8,c("log S_k in data","log S_k uniform"),lty=1:2,cex=0.7)

#######EXERCISE 2
library(sn)



m1=matrix(, nrow=150,ncol=100)
m2=matrix(, nrow=150,ncol=100)
for (i in 1:100){

v1 <- c(rnorm(50,0,1), rsn(70,5,1,8), rnorm(30,6,1))
v2 <- c(rnorm(50,0,1), rsn(70,0,1,8), 8+rt(30,5))
m1[,i]=v1
m2[,i]=v2
}



kmax=10

set.seed(123)
clus.PCA = list()
clus.original=list()
pb=txtProgressBar(min=0,max=100,style=3)
for(i in 1:100){
  clus.PCA[[i]]=clusGap(cbind(m1[,i],m2[,i]),kmeans,K.max = kmax,B=100,d.power=2,spaceH0="scaledPCA",nstart=100,verbose = F)
  clus.original[[i]]=clusGap(cbind(m1[,i],m2[,i]),kmeans,K.max = kmax,B=100,d.power=2,spaceH0="original",nstart=100,verbose = F)
  setTxtProgressBar(pb,i)
  
}

gap.PCA=list()
for(i in 1:100){
  gap.PCA[[i]]=clus.PCA[[i]]$Tab[,3]
}


sd.PCA=list()
for(i in 1:100){
  sd.PCA[[i]]=clus.PCA[[i]]$Tab[,4]
}

gap_diff.tib= function(x,y,q){
  n=length(x)
  diff=c()
  for (i in 2:n){
    diff[i]=x[i-1]-(x[i]-q*y[i])
  }
  return(diff[-1])
}

gap.difftib.PCA=sapply(1:100, 
                function(i){gap_diff.tib(gap.PCA[[i]],sd.PCA[[i]],1)})



number.clusters=function(x,kmax){
  b=kmax-1
  for (i in 1:b){
    if(x[i]>0){
      
      a=i
      break
      
    } else{
      a=kmax
    }
  }
  return(a)
}

number_clusters.tib.PCA=sapply(1:100, 
                               function(i){number.clusters(gap.difftib.PCA[,i],kmax)})



###########globalSEmax
gap_diff.global= function(x,y,q){
  n=length(x)
  diff=c()
  for (i in 1:n){
    if (max(x)==x[i]){
      m=i
    }
  }
  for (i in 2:n){
    diff[i]=x[i-1]-(x[m]-q*y[m])
  }
  return(diff[-1])
}

gap.diffglob.PCA=sapply(1:100, 
                       function(i){gap_diff.global(gap.PCA[[i]],sd.PCA[[i]],1)})

number_clusters.global.PCA=sapply(1:100, 
                               function(i){number.clusters(gap.diffglob.PCA[,i],kmax)})


#####original

gap.original=list()
for(i in 1:100){
  gap.original[[i]]=clus.original[[i]]$Tab[,3]
}


sd.original=list()
for(i in 1:100){
  sd.original[[i]]=clus.original[[i]]$Tab[,4]
}

gap.difftib.original=sapply(1:100, 
                       function(i){gap_diff.tib(gap.original[[i]],sd.original[[i]],1)})


gap.diffglob.original=sapply(1:100, 
                        function(i){gap_diff.global(gap.original[[i]],sd.original[[i]],1)})

number_clusters.global.original=sapply(1:100, 
                                  function(i){number.clusters(gap.diffglob.original[,i],kmax)})


number_clusters.tib.original=sapply(1:100, 
                               function(i){number.clusters(gap.difftib.original[,i],kmax)})

#######evaluations

table(number_clusters.tib.original)
table(number_clusters.global.original)
table(number_clusters.tib.PCA)
table(number_clusters.global.PCA)

###EX3
library(MASS)
set.seed(123)
cor.dis <- function(a,b){1 - abs(cor(a,b))}

Sigma    <- matrix(c(1,0,0,1), nrow=2) # covariance matrix of X and Z
matrixXZ <- mvrnorm(n=100, mu=c(0,0), Sigma=Sigma)
X <- matrixXZ[,1] # mean 0, variance 1
Y <- matrixXZ[,2] # mean 0, variance 1
cor(X,Y) # nearly zero
Z <- X + Y

cor.dis(X,Z) 
# 0.2928932
cor.dis(Y,Z)
# 0.2928932
d1(X,Y)
# 1
d1(X,Y) <= d1(Z,Y) + d1(X,Z)
