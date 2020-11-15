stickers = function( N = 40 ) {
  x = sample( 1 : 20, N, replace = T )
  arr = rep.int(F,20)
  for(i in 1:N) {
    arr[x[i]] = T
  }
  for(i in 1:20) {
    if(arr[i]==F) return(F);
  }
  return(T);
}

rep.stickers = function( n ) {
  c = 0
  for( i in 1 : n)
    c = c + stickers()
  return(c)
}
