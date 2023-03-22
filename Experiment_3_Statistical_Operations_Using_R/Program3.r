# Ashmin Jayson S6DS Roll No : 14
# Program 3 : Statistical Operation on dataframes
# 15th March 2023

print('Enter the data points and ENTER to stop input')  
name <- strsplit(readline('Enter the names : '), ' ')
name <- unlist(name)
age <- strsplit(readline('Enter the ages : '), ' ')
age <- as.numeric(unlist(age))
height <- strsplit(readline('Enter the heights : '), ' ')
height <- as.numeric(unlist(height))
weight <- strsplit(readline('Enter the weights : '), ' ')
weight <- as.numeric(unlist(weight))


df <- data.frame(name = name, age = age, height = height, weight = weight)
num_cols <- unlist(lapply(df, is.numeric))
ndf <- df[, num_cols]

modecalc <- 
  function(v) {
  return(names(sort(-table(v)))[1])
}


ndfcols <- c()
for (col in colnames(ndf)) {
  
  ndfcols <- c(ndfcols, col)
  
  cat('\n\n\nColumn Name :', col, 'Results of statistical operations : ')
  cat('\nMEAN:', mean(ndf[[col]]))
  cat('\nMEDIAN:', median(ndf[[col]]))
  cat('\nMODE:', modecalc(ndf[[col]]))
  cat('\nSTANDARD DEVIATION:', sd(ndf[[col]]))
  cat('\nFIVE NUMBER SUMMARY:', fivenum(ndf[[col]]))
  cat('\nBOX PLOT ON RIGHT')
  boxplot(ndf[[col]])

}

cat('\n\nCorrelation coefficient of Numeric Attributes is : \n')
print(cor(ndf[, ndfcols]))









