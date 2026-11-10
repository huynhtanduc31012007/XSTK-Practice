#Tính xác suất bẻ một que thành 3 đoạn tạo nên một bộ tam giác
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




# Tính xác suất chọn một điểm trong hình tròn bán kính bằng 1 sao cho điểm đó nằm trong hình vuông nội tiếp

PhepThu = function(){
  res = runif(2, -1, 1)
  if (res[1]^2 + res[2]^2 <= 1)
    return (res)
  else 
    PhepThu()
}

B = function(){
  p = PhepThu()
  return (abs(p[1]) <= 1 /sqrt(2) & abs(p[2]) <= 1/sqrt(2))
}
n = 10000
BienCoB = replicate(n, B())
P_B = mean(BienCoB)
cat(P_B) 



#  Sinh ra 52 lá bài gồm 13 quân bài, mỗi quân bài có 3 chất cơ rô chuồng bích, rút ngẫu nhiên 5 lá từ bộ bài
#  hoàn lại, tính xác suất trong 5 lá có đúng 2 lá cùng số khác chất.

#Cách 1
PhepThu = function(){
  sample(x = 1:52, size = 5, replace = FALSE)
}
BienCo = function(){
  V = PhepThu()
  for (i in 1:5){
    V[i] = V[i] %/% 4 + (V[i] %% 4 != 0)
  }
  V = sort(V)
  for (i in 1:4){
    if (V[i] == V[i + 1])
      return (TRUE)
  }
  return (FALSE)
}

#Cách 2
BienCo = function() {
  V = sample(1:52, size = 5, FALSE)
  V = ceiling(V / 4) 
  return(anyDuplicated(V) > 0)
}

n = 10000
simp = replicate(n, BienCo())
cat(mean(simp))


#Bài 5: Chọn ngẫu nhiên 5 số từ tập hợp các số nguyên từ 1 đến 20, không hoàn lại. Tính XS có ít nhất 2 số liên tiếp

BienCo = function(){
  chooses = sample(x = 1:20, 5)
  chooses = sort(chooses)
  for (i in 1:4){
    if (chooses[i] + 1 == chooses[i + 1])
      return (TRUE)
  }
  return (FALSE)
}

n = 10000
cat(mean(replicate(n, BienCo())))








