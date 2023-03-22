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
df2 <- df[!duplicated(df), ]
cat('The dataframe without duplicate rows is :')
print(df2)

rs = rowSums(df2)
maxrs = max(rs)
cat('The max row sum is : ', maxrs)
cat('\nThe factorial of max row sum is : ', fact(maxrs))