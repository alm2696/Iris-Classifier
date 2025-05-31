# Read the Iris dataset
iris_data <- read.csv("C:\\Users\\angel\\Downloads\\Iris-Data.csv")

# Display column names and the head of the dataset
cat("Column Names:\n")
print(colnames(iris_data))

cat("\nFirst 6 rows of the dataset:\n")
print(head(iris_data))

# Compute averages of each attribute for each class
numeric_columns <- sapply(iris_data, is.numeric)

averages <- aggregate(. ~ Class, data = iris_data[, c(which(numeric_columns), which(names(iris_data) == "Class"))], FUN = mean)

cat("\nAverages for each attribute by class:\n")
print(averages)

# Build a classifier based on attribute comparisons with averages
# Create a function for classification
classify_iris <- function(row) {
  # Get the species for this row
  class <- row["Class"]
  
  # Determine the average values for the species
  avg_row <- averages[averages$Class == class, ]
  
  # Initialize classification result
  classification <- "Unclassified"
  
  # Apply logical conditions based on attribute comparisons
  if (row["Attr1"] > avg_row["Attr1"] &
      row["Attr3"] < avg_row["Attr3"]) {
    classification <- "Iris-setosa"
  } else if (row["Attr2"] > avg_row["Attr2"] &
             row["Attr4"] < avg_row["Attr4"]) {
    classification <- "Iris-versicolor"
  } else {
    classification <- "Iris-virginica"
  }
  
  return(classification)
}

# Apply the classifier to the dataset and store results
iris_data$PredictedClass <- apply(iris_data, 1, classify_iris)

# Show the results of the classification
cat("\nClassification Results:\n")
print(head(iris_data[, c("Attr1", "Attr2", "Class", "PredictedClass")]))

# Evaluate classifier performance using confusion matrix
library(caret)
conf_matrix <- confusionMatrix(factor(iris_data$PredictedClass), factor(iris_data$Class))
cat("\nConfusion Matrix:\n")
print(conf_matrix)
