
# descriptive statistics with R
# A statistical analysis for the data in iris.csv

# some notes:

# note that only for the second variable the histogram indicates Gaussian distribution,
# confirmed by normality test (all the rest are non-parametric).

# note also the strong positive correlation indicated by scatter plots between 3rd and 4th scale variable,
# confirmed by Spearman's correlation coefficient = 0.9627571 (we are in the non-parametric case).

# -------------------------------------------------------------

# DATAFRAME: --------------------------------------------------
# load data to a dataframe type:
data <- read.csv("/path/tofile/iris.csv")

# use the following command to see the variables and their data type:
str(data)

# to get the names of the variables: (R creates names automatically if not defined otherwise)
colNames = names(data)

# size of dataframe:
nCol <- ncol(data)
nRow <- nrow(data)
#print(nCol)

# get the values of the categorical variable
# as dataframe:
#y_df = data[colNames[5]]
#print(class(y_df))

# or as a vector (here factor):
y_v = data[[colNames[5]]]
print(class(y_v))
#print(y_v)

cat("\n\n")

summary(data)

# BOXPLOTS: ---------------------------------------------------

# Create the boxplot file (pdf, png, etc..)
pdf(file = "boxplot.pdf")

# Boxplots
for (i in 1:(nCol-1))
{
	boxplot(data[[colNames[i]]] ~ data[[colNames[5]]], data = data, xlab = "classification",
   ylab = "characterization values")
}

# Save the file
dev.off()




# HISTOGRAMS: -------------------------------------------------
# we plot the variable histograms to visualize their distribution.

# if needed add the following in order to save plots in pdf, png, etc file:
pdf(file = "histogram.pdf")

# Create the histogram
for (i in 1:(nCol-1))
{
	hist(data[[colNames[i]]], xlab = "characterization values", col = "red", border = "blue")
}

# Save the file
dev.off()



# NORMALITY testing for the above scale variables: ------------
cat("\n\n")
print("----------------")
print("Shapiro-Wilk tests of Normality:")
cat("\n\n")

for (i in 1:(nCol-1))
{
	cat("Results for ", colNames[i])
	print(shapiro.test(data[[colNames[i]]]))
	cat("\n\n")
}
print("----------------")




# SCATTERPLOT: ------------------------------------------------
# The scatter plots will demonstrate correlations between the scale variables:
# Give the chart file a name.
pdf(file = "scatterplot_matrices.pdf")

# Plot the matrices between 4 variables giving 12 plots.
# One variable with 3 others and total 4 variables.

pairs(~data[[colNames[1]]]+data[[colNames[2]]]+data[[colNames[3]]]+data[[colNames[4]]],data = data,
   main = "Scatterplot Matrix")

# Save the file.
dev.off()


# CORRELATION coefficient (Spearman): -------------------------

cat("\n\nBelow are all the pairwise Spearman correlation coefficients: \n\n")

for (i in 1:(nCol-2))
{
	for (j in (i+1):(nCol-1))
	{
		rSp <- cor(data[[colNames[i]]], data[[colNames[j]]],  method = "pearson", use="pairwise.complete.obs")

		cat("The Spearman correlation coefficient for the ", i , " and ", j ," scale variable is: ", rSp, "\n\n")
	}
}



