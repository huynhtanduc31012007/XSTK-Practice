PhepThu = function(){
  res = runif(2, 0, 1)
  if (res[1] > 0 & res[1] < res[2] & res[2] < 1)
      return (res)
  else 
      PhepThu()
}
A = function(){
  p = PhepThu()
  L1 = p[1]
  L2 = p[2] - p[1]
  L3 = 1 - p[2]
  return (L1 < L2 + L3 & L2 < L1 + L3 & L3 < L1 + L2)
}

n = 10000
BienCoA = replicate(n, A())
P_A = mean(BienCoA)
cat(P_A)










