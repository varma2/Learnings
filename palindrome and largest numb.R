sum = 0
vec = seq(1,999,1)
for ( x in vec) { 
  if(x %% 3 == 0) | if (x %%5 == 0)
  sum = sum + x
}
sum <-0 
for (x in vec) {
  if( x %% 3 ==0 || x %% 5 == 0)
    sum = sum + x
}

sum

a <- sum(seq(0,1000,3))
b <- sum(seq(0,1000,5))
c <- sum(seq(0,1000,15))

d <- a + b - c
d


is.palindrome <- function(word) {
  rawWord <- charToRaw(tolower(word)) ## converts to lower case
  sprintf("%s is %sa palindrome", word,
          c("not ", "")[identical(rawWord, rev(rawWord)) + 1])
}

is.palindrome("1221")


identical(123,123)
identical(123,rev(321))
rev('321')
rev(strsplit("321",'')[[1]])
paste(rev(strsplit("321",'')[[1]]), collapse ="")

num = as.character(124521)
identical(num, paste(rev(strsplit(num,'')[[1]]), collapse =""))


for(x in seq(999, 900, -1)) {
  for (y in seq(999, 900, -1)) {
    num = x * y
    numb = as.character(num)
    revnumb = paste(rev(strsplit(numb, '')[[1]]), collapse = "")
    #print (c(x, y, num))
    a = identical(numb, revnumb)
    if (a == TRUE)
      print (c(x, y, numb, revnumb, a))
    
  }
  break
}

num
