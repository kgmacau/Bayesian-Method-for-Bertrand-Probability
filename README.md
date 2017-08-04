# Bayesian-Method-for-Bertrand-Probability
Aim to calculate Bertrand Probability (random-endpoint, random-radius, random-midpoint and Bayesian method) using simulation method with R


########################################;

###  Bertrand
probability Paradox   ####;

########################################;

 

 

rm(list=ls(all=TRUE))

 

 

## Method 1: Random Endpoint

random_endpoint<- function(simulation){

  n=0;

  r=1;

  for (i in
1:simulation){

    

     
t1<-runif(1,0,2*pi) ;

     
x1=r*sin(t1);

     
y1=r*cos(t1);

  

     
t2<-runif(1,0,2*pi) ;

     
x2=r*sin(t2);

     
y2=r*cos(t2);

  

     
distance<-(x1-x2)^2+(y1-y2)^2

     
r1=x1^2+y1^2;

     
r2=x2^2+y2^2;

  

      if
(distance>3) n=n+1

  

      prob=n/i;

  

     
ds<-cbind(i, n, prob)

  }

  show(ds)

}

 

 

 

## Method 2: Random Radius

random_radius<- function(simulation){

  n=0;

  r=1;

  for (i in
1:simulation){

  

      # step 1 

      t<-runif(1,0,2*pi)
;

     
x1=r*sin(t);

     
y1=r*cos(t);

  

      #step 2

      x2=-x1;

      y2=-y1;

  

      #step 3

     
x3=runif(1, -abs(x1), abs(x2));

     
if(x1/y1<0) y3=(abs(x2)-x3)*abs(y1/x1)-abs(y2)

      else if
(x1/y1>0) y3=(x3+abs(x2))*abs(y1/x1)-abs(y2)

  

      #step 4

     
distance<-(2*sqrt(1-x3^2-y3^2))^2

  

      if
(distance>3) n=n+1

  

      prob=n/i;

  

     
ds<-cbind(i, n, prob, x3, y3)

  }

 

show(ds)

}

 

 

 

 

## Mehotd 3: Random Midpoint

random_midpoint<- function(simulation){

  n=0;

  for (i in
1:simulation){

    

    # step 1 

   
t<-runif(1,0,2*pi);

   
r<-runif(1,0,1);

   
x=sqrt(r)*sin(t);

   
y=sqrt(r)*cos(t);

    

    #step 2

   
distance<-(2*sqrt(1-x^2-y^2))^2

    

    if
(distance>3) n=n+1

    

    prob=n/i;

    

   
ds<-cbind(i, n, prob, x, y)

  }

  

  show(ds)

}

 

random_endpoint(simulation=100000)

random_radius( 
simulation=1000000)

random_midpoint(simulation=100000)

 

 

 

 

## Method 4: Bayesian Conditional probability

Bayesian_berparadox<-function(simulation, p1, p2,
p3){

 

    r=1; n=0;
n1=0; n2=0; n3=0; m=0; m1=0; m2=0; m3=0;

    method <-
c(1, 2, 3) ## 1 random_endpoint, 2 random_radius, 3 rando_midpoint

    simu <-
sample(method, size=simulation, replace=TRUE, prob=c(p1, p2, p3))

   
one<-length(which(simu == 1))

   
two<-length(which(simu == 2))

   
three<-length(which(simu == 3))

    

    for (i in
1:one){

         # step
1

        
t1<-runif(1,0,2*pi) ;

        
x1=r*sin(t1);

        
y1=r*cos(t1);

         

         # step
2

        
t2<-runif(1,0,2*pi) ;

        
x2=r*sin(t2);

        
y2=r*cos(t2);

      

         # step
3

        
distance<-(x1-x2)^2+(y1-y2)^2;

         if
(distance>3) n1=n1+1;   

        
m1=m1+1;

        
prob1=n1/m1;   

    }

    for (i in
1:two){

         # step
1 

        
t<-runif(1,0,2*pi);

        
x1=r*sin(t);

        
y1=r*cos(t);

  

         #step 2

         x2=-x1;

         y2=-y1;

  

         #step 3

        
x3=runif(1, -abs(x1), abs(x2));

         if(x1/y1<0)
y3=(abs(x2)-x3)*abs(y1/x1)-abs(y2)

         else if
(x1/y1>0) y3=(x3+abs(x2))*abs(y1/x1)-abs(y2)

  

         #step 4

        
distance<-(2*sqrt(1-x3^2-y3^2))^2;

         if
(distance>3) n2=n2+1;  

        
m2=m2+1;

        
prob2=n2/m2;

    }

    for (i in
1:three){

         # step
1 

        
t<-runif(1,0,2*pi);

        
r<-runif(1,0,1);

        
x=sqrt(r)*sin(t);

        
y=sqrt(r)*cos(t);

    

         #step 2

        
distance<-(2*sqrt(1-x^2-y^2))^2;

         if
(distance>3) n3=n3+1;

        
m3=m3+1;

        
prob3=n3/m3;   

    }

 

   
n=n1+n2+n3;  

    m=m1+m2+m3;

   
prob=n/simulation;

   
ds<-cbind(prob, n, n1, n2, n3, m, m1, m2, m3, prob1, prob2, prob3); 

    return(ds)

}

 

Bayesian_berparadox(simulation=100000, p1=1/3, p2=1/3,
p3=1/3);

Bayesian_berparadox(simulation=100000, p1=1/2, p2=1/2,
p3=0);

Bayesian_berparadox(simulation=100000, p1=1/4, p2=1/4,
p3=1/2);

