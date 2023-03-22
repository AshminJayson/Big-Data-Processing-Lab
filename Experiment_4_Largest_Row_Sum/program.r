df = read.table("./data.txt", sep=",", header = TRUE)

fact = function(x) {
    if (x <= 1) {
        return(1)
    }

    return (x * fact(x - 1))
}

cat('The input dataframe is :')
print(df)

df = sapply(df, as.numeric)

x = nrow(df)
i = 0
while (i <= x) {
  j = i + 1
  while(j <= x) {
    if (setequal(df[j, ], df[i, ])) {
      df = df[-j, ]
      x = x - 1
    }
    else {
    j = j + 1
    }
  }
  i = i + 1
}

cat('The dataframe without duplicate rows is :')
print(df)

rs = rowSums(df)
maxrs = max(rs)
cat('The max row sum is : ', maxrs)
cat('\nThe factorial of max row sum is : ', fact(maxrs))

