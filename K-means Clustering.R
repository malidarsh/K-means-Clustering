###2 means clustering example 1
rm(list=ls())
x=c(0,0,2,0,3,3)
y=c(0,2,0,1,0,1)
n=length(x)   ###Total number of Data points
k=2   ###2 Means clustering
Data=as.matrix(cbind(x,y))
###Plot for seed guess
plot(Data[,1],Data[,2],xlim=c(-2,6),ylim=c(-1,3))   ###Scatter plot of Data
seed=t(as.matrix(cbind(c(0,0.5),c(3,0.5))))   ###Initial Seed

distn=function(x,y,a,b){   
  distance=(abs(x-a)+abs(y-b))   ###Manhattan Distance function
  return(distance)
}

Dmat=matrix(0,length(x),k) ###Matrix for storing distance values

index=F

if(k==2){
  while(index==F){
    clus1=matrix(0,n,2)
    clus2=matrix(0,n,2)
    for(i in 1:length(x)){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    Dmat
    clus1=subset(Data,Dmat[,1]<=Dmat[,2])
    clus2=subset(Data,Dmat[,1]>Dmat[,2])
    a1=apply(clus1,2,mean)
    b1=apply(clus2,2,mean)
    seed=t(as.matrix(cbind(c(a1[1],a1[2]),c(b1[1],b1[2]))))
    clustp1=clus1
    clustp2=clus2
    if(identical(clustp1,clus1)==1 && identical(clustp2,clus2)==1){
      index=T
    }
    centroids=seed
  }
  final_cluster=list()
  final_cluster[[1]]=clustp1
  final_cluster[[2]]=clustp2
  
}else{
  while(index==F){
    clus=list()
    for(i in 1:k){
      clus[[i]]=matrix(NA,n,2)
    }
    
    for(i in 1:n){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    minval=c()
    C=matrix(NA,n,k)
    CC=matrix(NA,n,k)
    for(i in 1:nrow(C)){
      for(j in 1:ncol(C)){
        minval[i]=which.min(Dmat[i,])
        min=minval[i]
        if(j==min){
          C[i,j]=Data[i,1]
          CC[i,j]=Data[i,2]
          kkk=cbind(C,CC)
        }
      }
    }
    for(i in 1:k){
      clus[[i]]=cbind(kkk[,i],kkk[,k+i])
    }
    
    alpha=list()
    for(i in 1:k){
      alpha[[i]]=apply(na.omit(clus[[i]]),2,mean)
    }
    
    temp=cbind(c(alpha[[1]][1],alpha[[1]][2]),c(alpha[[2]][1],alpha[[2]][2]))
    for(i in 3:k){
      temp=cbind(temp,c(alpha[[i]][1],alpha[[i]][2]))
    }
    
    seed=t(as.matrix(temp))
    clustp=clus
    
    if(identical(clustp,clus)){
      index=T
    }
    centroids=seed
  }
  
  
  final_cluster=list()
  for(i in 1:k){
    final_cluster[[i]]=na.omit(clustp[[i]])
  }
  
}


#final_cluster
centroids  ###Final Centroids

plot(final_cluster[[1]][,1],final_cluster[[1]][,2],pch=13,xlim=c(-2,6),ylim=c(-1,3),col="red",main="2 Means Clustering",xlab = "X",ylab="Y")
lines(final_cluster[[2]][,1],final_cluster[[2]][,2],pch=16,xlim=c(-2,6),ylim=c(-1,3),type="p",col="blue")
lines(centroids[,1],centroids[,2],pch=4,type="p",cex=3)
legend("topright",legend=c("first cluster","second cluster"),pch = c(13,16),col=c("red","blue"))



####2 means clustering example 2
rm(list=ls())
k=2   ###2 Means clustering
x=cbind(rnorm(1000),runif(1000))
y=cbind(runif(1000,10,11),rnorm(1000))
n=length(x)   ###Total number of Data points
Data=as.matrix(rbind(x,y))
###Plot for seed guess
plot(Data[,1],Data[,2],xlim=c(-2,12),ylim=c(-3,3))   ###Scatter plot of Data
seed=t(as.matrix(cbind(c(0,0.5),c(11,-1))))    ###Initial Seed
seedprime=seed
distn=function(x,y,a,b){   
  distance=(abs(x-a)+abs(y-b))   ###Manhattan Distance function
  return(distance)
}

Dmat=matrix(0,length(x),k) ###Matrix for storing distance values

index=F

if(k==2){
  while(index==F){
    clus1=matrix(0,n,2)
    clus2=matrix(0,n,2)
    for(i in 1:length(x)){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    Dmat
    clus1=subset(Data,Dmat[,1]<=Dmat[,2])
    clus2=subset(Data,Dmat[,1]>Dmat[,2])
    a1=apply(clus1,2,mean)
    b1=apply(clus2,2,mean)
    seed=t(as.matrix(cbind(c(a1[1],a1[2]),c(b1[1],b1[2]))))
    clustp1=clus1
    clustp2=clus2
    if(identical(clustp1,clus1)==1 && identical(clustp2,clus2)==1){
      index=T
    }
    centroids=seed
  }
  final_cluster=list()
  final_cluster[[1]]=clustp1
  final_cluster[[2]]=clustp2
  
}else{
  while(index==F){
    clus=list()
    for(i in 1:k){
      clus[[i]]=matrix(NA,n,2)
    }
    
    for(i in 1:n){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    minval=c()
    C=matrix(NA,n,k)
    CC=matrix(NA,n,k)
    for(i in 1:nrow(C)){
      for(j in 1:ncol(C)){
        minval[i]=which.min(Dmat[i,])
        min=minval[i]
        if(j==min){
          C[i,j]=Data[i,1]
          CC[i,j]=Data[i,2]
          kkk=cbind(C,CC)
        }
      }
    }
    for(i in 1:k){
      clus[[i]]=cbind(kkk[,i],kkk[,k+i])
    }
    
    alpha=list()
    for(i in 1:k){
      alpha[[i]]=apply(na.omit(clus[[i]]),2,mean)
    }
    
    temp=cbind(c(alpha[[1]][1],alpha[[1]][2]),c(alpha[[2]][1],alpha[[2]][2]))
    for(i in 3:k){
      temp=cbind(temp,c(alpha[[i]][1],alpha[[i]][2]))
    }
    
    seed=t(as.matrix(temp))
    clustp=clus
    
    if(identical(clustp,clus)){
      index=T
    }
    centroids=seed
  }
  
  
  final_cluster=list()
  for(i in 1:k){
    final_cluster[[i]]=na.omit(clustp[[i]])
  }
  
}


#final_cluster
centroids  ###Final Centroids

plot(clustp1[,1],clustp1[,2],pch=13,xlim=c(-2,12),ylim=c(-3,3),col="red",main="2 Means Clustering",xlab = "X",ylab="Y")
lines(clustp2[,1],clustp2[,2],pch=16,xlim=c(-2,12),ylim=c(-3,3),type="p",col="blue")
lines(centroids[,1],centroids[,2],pch=4,type="p",cex=3)
legend("topleft",legend=c("first cluster","second cluster","centroids"),cex=0.8,pch = c(13,16,4),col=c("red","blue","black"))


command=kmeans(Data, seedprime)$"centers"
result=data.frame("X-coordinate of seed from above algorithm"=centroids[,1],"Y-coordinate of seed from above algorithm"=centroids[,2],"X-coordinate of seed from built in algorithm"=command[,1],"Y-coordinate of seed from built in algorithm"=command[,2])
result


##########
###k means clutering First Example for k=4
rm(list=ls())
x=c(5,5,3,0,2,4,2,2,1,5)
y=c(0,2,1,4,1,2,2,3,3,4)
n=length(x)
k=4###Number of cluster which we want
Data=as.matrix(cbind(x,y))
plot(Data[,1],Data[,2],xlim=c(-1,6),ylim=c(-1,5))
seed=t(as.matrix(cbind(c(1,1),c(2.5,1),c(5,1),c(5,3))))
seed

if(nrow(seed)!=k){
  print("Please check Seed vaules and number of cluster")}
seedprime=seed


distn=function(x,y,a,b){
  distance=(abs(x-a)+abs(y-b))
  return(distance)
}

Dmat=matrix(0,n,k)
index=F


if(k==2){
  while(index==F){
    clus1=matrix(0,n,2)
    clus2=matrix(0,n,2)
    for(i in 1:length(x)){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    Dmat
    clus1=subset(Data,Dmat[,1]<=Dmat[,2])
    clus2=subset(Data,Dmat[,1]>Dmat[,2])
    a1=apply(clus1,2,mean)
    b1=apply(clus2,2,mean)
    seed=t(as.matrix(cbind(c(a1[1],a1[2]),c(b1[1],b1[2]))))
    clustp1=clus1
    clustp2=clus2
    if(identical(clustp1,clus1)==1 && identical(clustp2,clus2)==1){
      index=T
    }
    centroids=seed
  }
  
}else{
  while(index==F){
    clus=list()
    for(i in 1:k){
      clus[[i]]=matrix(NA,n,2)
    }
    
    for(i in 1:n){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    minval=c()
    C=matrix(NA,n,k)
    CC=matrix(NA,n,k)
    for(i in 1:nrow(C)){
      for(j in 1:ncol(C)){
        minval[i]=which.min(Dmat[i,])
        min=minval[i]
        if(j==min){
          C[i,j]=Data[i,1]
          CC[i,j]=Data[i,2]
          kkk=cbind(C,CC)
        }
      }
    }
    for(i in 1:k){
      clus[[i]]=cbind(kkk[,i],kkk[,k+i])
    }
    
    alpha=list()
    for(i in 1:k){
      alpha[[i]]=apply(na.omit(clus[[i]]),2,mean)
    }
    
    temp=cbind(c(alpha[[1]][1],alpha[[1]][2]),c(alpha[[2]][1],alpha[[2]][2]))
    for(i in 3:k){
      temp=cbind(temp,c(alpha[[i]][1],alpha[[i]][2]))
    }
    
    seed=t(as.matrix(temp))
    clustp=clus
    
    if(identical(clustp,clus)){
      index=T
    }
    centroids=seed
  }
  
  
  final_cluster=list()
  for(i in 1:k){
    final_cluster[[i]]=na.omit(clustp[[i]])
  }
  
}


#final_cluster
centroids  ###Final Centroids


###Plotting
plot(final_cluster[[1]][,1],final_cluster[[1]][,2],pch=13,xlim=c(-1,6),ylim=c(-1,5),col="red",main="k Means Clustering",xlab = "X",ylab="Y")
lines(final_cluster[[2]][,1],final_cluster[[2]][,2],pch=16,xlim=c(-1,6),ylim=c(-1,5),type="p",col="blue")
lines(final_cluster[[3]][,1],final_cluster[[3]][,2],pch=18,xlim=c(-1,6),ylim=c(-1,5),type="p",col="green")
lines(final_cluster[[4]][,1],final_cluster[[4]][,2],pch=20,xlim=c(-1,6),ylim=c(-1,5),type="p",col="orange")
lines(centroids[,1],centroids[,2],pch=4,type="p",cex=2)
legend("bottomleft",legend=c("first cluster","second cluster","third cluster","fourth cluster","centroids"),cex=0.8,pch = c(13,16,18,20,4),col=c("red","blue","green","orange","black"))



########## k means clustering example 4 for k=3
rm(list=ls())
x=cbind(rnorm(1000,5),runif(1000),runif(1000,3,5))
y=cbind(runif(1000,10,11),rnorm(1000),rnorm(1000,3,0.5))
n=length(x)
k=3
Data=as.matrix(rbind(cbind(x[,1],y[,1]),cbind(x[,2],y[,2]),cbind(x[,3],y[,3])))

plot(Data[,1],Data[,2],xlim=c(-1,7),ylim=c(-3,12))

seed=t(as.matrix(cbind(c(0.5,0.1),c(4,3.5),c(6,10))))
seed
if(nrow(seed)!=k){
  print("Please enter Seed vaules and number of cluster")}
seedprime=seed  ###For cross checking our algoithm agains built in command
distn=function(x,y,a,b){
  distance=(abs(x-a)+abs(y-b))
  return(distance)
}
Dmat=matrix(0,n,k)
index=F


if(k==2){
  while(index==F){
    clus1=matrix(0,n,2)
    clus2=matrix(0,n,2)
    for(i in 1:length(x)){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    Dmat
    clus1=subset(Data,Dmat[,1]<=Dmat[,2])
    clus2=subset(Data,Dmat[,1]>Dmat[,2])
    a1=apply(clus1,2,mean)
    b1=apply(clus2,2,mean)
    seed=t(as.matrix(cbind(c(a1[1],a1[2]),c(b1[1],b1[2]))))
    clustp1=clus1
    clustp2=clus2
    if(identical(clustp1,clus1)==1 && identical(clustp2,clus2)==1){
      index=T
    }
    centroids=seed
  }
  
}else{
  while(index==F){
    clus=list()
    for(i in 1:k){
      clus[[i]]=matrix(NA,n,2)
    }
    
    for(i in 1:n){
      for(j in 1:k){
        Dmat[i,j]=distn(Data[i,1],Data[i,2],seed[j,1],seed[j,2])
      }
    }
    minval=c()
    C=matrix(NA,n,k)
    CC=matrix(NA,n,k)
    for(i in 1:nrow(C)){
      for(j in 1:ncol(C)){
        minval[i]=which.min(Dmat[i,])
        min=minval[i]
        if(j==min){
          C[i,j]=Data[i,1]
          CC[i,j]=Data[i,2]
          kkk=cbind(C,CC)
        }
      }
    }
    for(i in 1:k){
      clus[[i]]=cbind(kkk[,i],kkk[,k+i])
    }
    
    alpha=list()
    for(i in 1:k){
      alpha[[i]]=apply(na.omit(clus[[i]]),2,mean)
    }
    
    temp=cbind(c(alpha[[1]][1],alpha[[1]][2]),c(alpha[[2]][1],alpha[[2]][2]))
    for(i in 3:k){
      temp=cbind(temp,c(alpha[[i]][1],alpha[[i]][2]))
    }
    
    seed=t(as.matrix(temp))
    clustp=clus
    
    if(identical(clustp,clus)){
      index=T
    }
    centroids=seed
  }
  
  
  final_cluster=list()
  for(i in 1:k){
    final_cluster[[i]]=na.omit(clustp[[i]])
  }
  
}

centroids  ###Final centroids

###Plotting
plot(final_cluster[[1]][,1],final_cluster[[1]][,2],pch=13,xlim=c(-1,7),ylim=c(-3,12),col="red",main="k Means Clustering",xlab = "X",ylab="Y")
lines(final_cluster[[2]][,1],final_cluster[[2]][,2],pch=16,xlim=c(-1,7),ylim=c(-3,12),type="p",col="blue")
lines(final_cluster[[3]][,1],final_cluster[[3]][,2],pch=18,xlim=c(-1,7),ylim=c(-3,12),type="p",col="green")
lines(centroids[,1],centroids[,2],pch=4,type="p",cex=2)
legend("topleft",legend=c("first cluster","second cluster","third cluster","centroids"),cex=0.8,pch = c(13,16,18,4),col=c("red","blue","green","black"))


###########
command=kmeans(Data, seedprime)$"centers"
result=data.frame("X-coordinate of seed from above algorithm"=centroids[,1],"Y-coordinate of seed from above algorithm"=centroids[,2],"X-coordinate of seed from built in algorithm"=command[,1],"Y-coordinate of seed from built in algorithm"=command[,2])
result

