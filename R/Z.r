twoTailed<-function(alpha,z){
    tab<-qnorm(alpha/2)
    tab<-round(tab,3)
    print(paste("Calculated value of z",z))
    print(paste("tabulated value of z",tab))
    if(-tab<z&z<tab){
        print("H0 is accepted")
    }
    else{
        print("H0 is rejected")
    }
}
oneTailLeft<-function(n,alpha,z){
    df<-n
    tab<-qnorm(alpha,lower.tail=TRUE)
    tab<-round(tab,3)
    print(paste("Calculated value of z",z))
    print(paste("tabulated value of z",tab))
    if(z<tab){
        print("H0 is accepted")
    }
    else{
        print("H0 is rejected")
    }
}
oneTailRight<-function(n,alpha,z){
    df<-n
    tab<-qnorm(alpha,lower.tail=FALSE)
    tab<-round(tab,3)
    print(paste("Calculated value of z",z))
    print(paste("tabulated value of z",tab))
    if(z>tab){
        print("H0 is rejected")
    }
    else{
        print("H0 is accepted")
    }
}
n<-as.numeric(readline("Enter sample size:"))
x<-as.numeric(readline("Enter sample mean:"))
u<-as.numeric(readline("Enter population mean:"))
s<-as.numeric(readline("Enter standard deviation:"))
alpha<-as.numeric(readline("Enter the level of significance(0.01/0.05/0.10)\n"))
z<-(x-u)/(s/sqrt(n))
z<-round(z,3)
opt<-as.numeric(readline("Chosse the type of Z test\n 1.Two Tailed \n 2.One TailLeft\n 3.One TailRight\n "))
switch(opt,
    "Case 1"=twoTailed(alpha,z),
    "Case 2"=oneTailLeft(n,alpha,z),
    "Case 3"=oneTailRight(n,alpha,z),
    {print("Invalid option")})

