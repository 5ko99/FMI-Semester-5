dice = function( N = 100 )
{
  x = sample( 1 : 6, N, replace = T )
  c = sum( x == 6 )
  return(c)
}
rep.dice = function( n )
{
  c = 0
  for( i in 1 : n)
    c = c + dice()
  return (c/n*100)
}

prob.dice = function( t )
{
  x = rep.int( 0, t )
  c = 0
  for(i in 1 : t )
  {
    c = c + dice()
    x[ i ] = c / ( 100 * i )
  }
  return(x)
}
birthdays = function( p = 0.5 )
{
  prob = 1;
  for(i in 1 : 365 )
  {
    prob = prob * (366-i) / 365
    if( prob < 1 - p ) break
  }
  return(i)
}

