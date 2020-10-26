birthdays = function(p)
{
  prob = 1;
  for(i in 1:365){
    prob = prob *(365-i)/365;
    if(prob < 1-p){
      break;
    }
  }
  return (i);
}
#sample(1:6,8,replace=T,prob=c(0.1,0.1,0.1,0.1,0.1,0.1))
#zad 2
dice = function(N = 100) {
  x = sample(1:6,N,replace=TRUE);
  print(x);
  out = sum(x==6);
  return(out);
}

rep.dice = function(n) {
  c = 0;
  for(i in 1:n) {
    c = c + dice();
  }
  return(c);
}

prob.dice = function(n) {
  x = rep.int(0,n);
  c = 0;
  for(i in 1:n) {
    c = c + dice();
    x[i] = c / (100*i);
  }
  return(x);
}
y=prob.dice(10000)
plot(y,type="l")
abline(h=1/6,col="red")

#zad 3
gameWithDad = function(p=0.3) {
  return(sample(c(0,1),1,prob = c(1-p,p)));
}

gameWithMom = function(p=0.4) {
  return(sample(c(0,1),1,prob = c(1-p,p)));
}

GameMDM = function( n = 3) {
  firstGameRes = gameWithMom();
  secondGameRes = gameWithDad();
  thirdGameRes = gameWithMom();
  res = firstGameRes+secondGameRes+thirdGameRes;
  
  if(res>=2 & secondGameRes==1){
    return(1);
  }
  return(0);
}

rep.GameMDM = function(n) {
  numberOfWins = 0;
  for(i in 1:n) {
    if(GameMDM()==1) {
      numberOfWins = numberOfWins + 1;
    }
  }
  return(numberOfWins);
}

prob.GameMDM = function(n) {
  x = rep.int(0,n);
  allWins = 0;
  for(i in 1:n) {
    allWins = allWins + rep.GameMDM(100);
    x[i]=allWins/(100*i);
  }
  return(x);
}
y = prob.GameMDM(100);
plot(y,type="l");
abline(h=0.192,col="red")

#zad 4
gifts = function(n = 20) {
  x = sample(1:n,20);
  y= 1:n;
  print(y);
  print(x);
  numberOfDiff = sum(x!=c(1:20));
  if(numberOfDiff==n) {
    return(1);
  } else {
    return(0);
  }
}

rep.gifts = function(n) {
  sum=0;
  for( i in 1:n) {
    sum = sum + gifts();
  }
  return(sum);
}

giftsNumberSelf = function(n=20) {
  x = sample(1:n);
  return(sum(x==c(1:n)));
}
rep.giftsNumberSelf = function(n) {
  sum = 0;
  for(i in 1:n) {
    sum = sum + giftsNumberSelf();
  }
  return(sum/n);
}
