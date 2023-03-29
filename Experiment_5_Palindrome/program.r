df1 = read.table("./d1.txt", sep=",", header = TRUE)
df2 = read.table("./d2.txt", sep=",", header = TRUE)

print('The input datasets are: ')
print(df1)
print(df2)

df2 = merge(df1, df2, by='id')
print(df2)