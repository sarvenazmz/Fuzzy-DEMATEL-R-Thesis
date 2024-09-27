# Load library
library(tidyverse)

# Define the TFN table
# The columns names are "Lingu", "l", "m", "u"
#then we have the values in the rows for each column
# dataframe for the table
# c( means they are filled in a row
triangular_fuzzy_numbers_table <- data.frame(
  Lingu = c("0", "VH", "HI", "LI", "VL", "NI"),
  l = c(0.00, 0.75, 0.50, 0.25, 0.00, 0.00),
  m = c(0.00, 1.00, 0.75, 0.50, 0.25, 0.00),
  u = c(0.00, 1.00, 1.00, 0.75, 0.50, 0.25)
)

# Define a list of matrices for the answers
# nrow=5 , with 25 elements, therefore, 5 columns
# c( means they are filled in a row
#fill the matrix by rows (row-major order):
#it place the first 5 elements in the first row
matrices <- list(
  
  matrix(c(
    "0", "VH", "HI", "HI", "LI",
    "VH", "0", "LI", "VH", "LI",
    "VH", "HI", "0", "HI", "HI",
    "HI", "LI", "LI", "0", "LI",
    "HI", "VH", "LI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "LI", "VL", "LI", "VL",
    "LI", "0", "NI", "VH", "NI",
    "VH", "HI", "0", "LI", "HI",
    "VL", "LI", "NI", "0", "LI",
    "VL", "HI", "VL", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "VH", "LI",
    "HI", "0", "HI", "VH", "HI",
    "VH", "VH", "0", "VH", "LI",
    "LI", "HI", "HI", "0", "LI",
    "HI", "VH", "LI", "LI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "LI", "HI", "HI",
    "VH", "0", "HI", "VH", "HI",
    "HI", "VH", "0", "HI", "VH",
    "VH", "VH", "LI", "0", "VH",
    "VH", "VH", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "VH", "VH", "VH", "VH",
    "VH", "0", "VH", "VH", "VH",
    "VH", "VH", "0", "VH", "VH",
    "VH", "VH", "VH", "0", "VH",
    "VH", "VH", "VH", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "NI", "HI", "NI", "NI",
    "VH", "0", "VH", "VH", "VH",
    "HI", "HI", "0", "HI", "LI",
    "HI", "HI", "HI", "0", "NI",
    "NI", "HI", "NI", "NI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "NI", "NI", "NI", "NI",
    "HI", "0", "HI", "VH", "HI",
    "HI", "HI", "0", "HI", "LI",
    "HI", "HI", "HI", "0", "NI",
    "NI", "HI", "NI", "NI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "HI",
    "VH", "0", "VH", "VH", "VH",
    "VH", "VH", "0", "LI", "VH",
    "LI", "LI", "LI", "0", "HI",
    "LI", "LI", "LI", "LI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "NI", "LI", "VH", "VH",
    "HI", "0", "LI", "VH", "HI",
    "VH", "NI", "0", "HI", "HI",
    "VL", "VL", "NI", "0", "VH",
    "VH", "LI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "NI", "LI", "HI", "VH",
    "HI", "0", "LI", "VH", "HI",
    "VH", "NI", "0", "HI", "HI",
    "VL", "VL", "NI", "0", "VH",
    "VH", "LI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "LI", "LI", "LI", "LI",
    "HI", "0", "VH", "VH", "VH",
    "VH", "HI", "0", "VH", "HI",
    "HI", "LI", "LI", "0", "HI",
    "HI", "VH", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "LI", "LI", "VH", "LI",
    "VH", "0", "HI", "HI", "HI",
    "VH", "HI", "0", "HI", "HI",
    "HI", "LI", "LI", "0", "HI",
    "LI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "LI", "LI", "VH", "LI",
    "VH", "0", "HI", "HI", "HI",
    "VH", "HI", "0", "LI", "LI",
    "HI", "LI", "LI", "0", "HI",
    "LI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "LI", "VH", "LI",
    "HI", "0", "LI", "VH", "LI",
    "HI", "HI", "0", "HI", "HI",
    "VL", "VL", "NI", "0", "NI",
    "VH", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "LI", "VH", "LI",
    "HI", "0", "LI", "VH", "LI",
    "HI", "LI", "0", "HI", "HI",
    "VL", "VL", "NI", "0", "NI",
    "VH", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "LI",
    "VH", "0", "VH", "HI", "LI",
    "VH", "HI", "0", "LI", "LI",
    "LI", "HI", "LI", "0", "HI",
    "HI", "VH", "LI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "VL", "VL", "VL", "VL",
    "HI", "0", "HI", "HI", "HI",
    "LI", "HI", "0", "VH", "HI",
    "HI", "HI", "HI", "0", "VH",
    "HI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "VH", "NI", "VH", "VH",
    "VH", "0", "NI", "VH", "VH",
    "VH", "VH", "0", "VH", "VH",
    "VH", "VH", "NI", "0", "VH",
    "VH", "VH", "NI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "VH", "HI", "LI", "HI",
    "VH", "0", "HI", "HI", "LI",
    "HI", "VH", "0", "VH", "HI",
    "VH", "LI", "VL", "0", "VH",
    "VL", "LI", "VL", "LI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "NI", "HI", "HI", "VH",
    "VH", "0", "NI", "HI", "HI",
    "VH", "VH", "0", "VH", "NI",
    "LI", "NI", "LI", "0", "HI",
    "VH", "VH", "VH", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "LI",
    "HI", "0", "HI", "HI", "LI",
    "HI", "HI", "0", "HI", "LI",
    "HI", "HI", "HI", "0", "HI",
    "HI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "LI",
    "HI", "0", "HI", "HI", "LI",
    "HI", "HI", "0", "HI", "LI",
    "HI", "HI", "HI", "0", "HI",
    "HI", "HI", "NI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "VH", "HI",
    "VH", "0", "LI", "HI", "LI",
    "VH", "LI", "0", "HI", "LI",
    "VH", "HI", "LI", "0", "VH",
    "VH", "LI", "HI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "VL", "HI", "VH",
    "VH", "0", "LI", "VH", "VH",
    "VH", "HI", "0", "VH", "VH",
    "HI", "VH", "HI", "0", "VH",
    "VH", "VH", "VH", "LI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "VH", "HI",
    "VH", "0", "VH", "HI", "LI",
    "VH", "HI", "0", "VH", "VH",
    "LI", "LI", "LI", "0", "LI",
    "LI", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "VH", "HI",
    "VH", "0", "VH", "HI", "LI",
    "VH", "HI", "0", "VH", "VH",
    "LI", "LI", "LI", "0", "LI",
    "LI", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "LI", "VL",
    "VH", "0", "VH", "LI", "HI",
    "VH", "VL", "0", "HI", "NI",
    "VL", "LI", "VL", "0", "VL",
    "HI", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "LI", "VL",
    "VH", "0", "NI", "LI", "HI",
    "VH", "VL", "0", "HI", "NI",
    "VL", "LI", "VL", "0", "VL",
    "HI", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "HI",
    "LI", "0", "LI", "LI", "LI",
    "HI", "HI", "0", "HI", "HI",
    "HI", "HI", "HI", "0", "HI",
    "HI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "HI", "HI", "HI",
    "LI", "0", "LI", "LI", "LI",
    "HI", "HI", "0", "HI", "HI",
    "HI", "HI", "HI", "0", "HI",
    "HI", "HI", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "VH", "LI", "LI", "VL",
    "HI", "0", "HI", "LI", "LI",
    "LI", "VH", "0", "HI", "HI",
    "LI", "HI", "VL", "0", "HI",
    "VL", "HI", "LI", "VH", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "VL", "HI", "VL",
    "VH", "0", "HI", "HI", "VL",
    "HI", "HI", "0", "VH", "LI",
    "HI", "LI", "HI", "0", "LI",
    "LI", "VH", "LI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "VL", "HI", "VL",
    "VH", "0", "HI", "HI", "VL",
    "HI", "HI", "0", "VH", "LI",
    "HI", "LI", "LI", "0", "LI",
    "LI", "VH", "LI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "LI", "LI", "HI",
    "VH", "0", "HI", "HI", "LI",
    "LI", "LI", "0", "LI", "VL",
    "HI", "HI", "LI", "0", "HI",
    "HI", "VH", "VL", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "LI", "HI", "VL",
    "VH", "0", "VH", "HI", "VH",
    "HI", "VL", "0", "VH", "HI",
    "LI", "LI", "LI", "0", "LI",
    "HI", "VH", "HI", "HI", "0"
  ), nrow = 5, byrow = TRUE),
  
  matrix(c(
    "0", "HI", "VH", "VH", "LI",
    "HI", "0", "NI", "VH", "NI",
    "VH", "LI", "0", "VH", "VH",
    "VL", "NI", "NI", "0", "VH",
    "NI", "NI", "VH", "VH", "0"
  ), nrow = 5, byrow = TRUE)
  
  
)


# Print the first matrix for verification
print(matrices[[2]])

#---------------------

# initial non-negative fuzzy direct-relation matrix

# Function to get TFN values based on the input value
# triangular_fuzzy_numbers_table$Lingu means the column of "Lingu"
# triangular_fuzzy_numbers_table$Lingu==value is TRUE if the value matches one of the rows
# for example "VH" etc
# 2:4 part means that only the second, third, and fourth columns are given to us
# as.numeric(tfn_values) converts the tfn_values to a numeric vector
# for example for "HI" it returns (0.50, 0.75, 1.00)
get_tfn <- function(value) {
  tfn_values <- triangular_fuzzy_numbers_table[triangular_fuzzy_numbers_table$Lingu == value, 2:4]
  return(as.numeric(tfn_values))
}

# Function to create 5x3 matrices for each barrier in the original matrix
# the whole of the each 5x3 matrix (tfn_matrices) for each barrier is one column in a bigger matrix "matrix" for each person
# For each iteration (i.e., for each column), we create a 5x3 matrix named tfn_matrix
# and we have 5 matrix of (5x3) for each barrier so it means 5 column in the bigger matrix
# if matrix[j, i] is "VH", then get_tfn("VH") would return the TFN for "VH", such as (0.75, 1.00, 1.00)
# In other words, for the current row j, the entire row of tfn_matrix (with 3 columns) is filled with the TFN for the linguistic term at matrix[j, i]
create_5x3_matrices <- function(matrix) {
  tfn_matrices <- list()
  # i here is column 
  for (i in 1:5) {
    tfn_matrix <- matrix(NA, nrow = 5, ncol = 3)
    # j here means row
    for (j in 1:5) {
      tfn_matrix[j, ] <- get_tfn(matrix[j, i]) 
    }
    tfn_matrices[[i]] <- tfn_matrix
  }
  return(tfn_matrices)
}

# Create 5x3 matrices for all matrices
# it is a nested list because you are putting a list of 36 matrices in the function
# The outer list (all_tfn_matrices) contains an element for each respondent or expert
# Each element in all_tfn_matrices (e.g. all_tfn_matrices[[36]]) is itself a list of 5x3 TFN matrices, one for each column (or barrier) in the original matrix provided by that expert
all_tfn_matrices <- lapply(matrices, create_5x3_matrices)

# initial non-negative fuzzy direct-relation matrix
print(all_tfn_matrices[[1]][[5]]) # A12 is the matrix of the first respondent for barrier 2

#-------------------

# Normalized fuzzy direct relation matrix

# Function to apply the transformation formula to a 5x3 matrix
transform_matrix <- function(tfn_matrix) {
  min_val <- min(tfn_matrix[, 1])
  max_val <- max(tfn_matrix[, 3])
  tfn_matrix <- (tfn_matrix - min_val) / (max_val - min_val)
  return(tfn_matrix)
}

# Apply the transformation to all TFN matrices
# since TFN matrices are in a nested list, the lapply function must be used twice (one for each level of nesting)
# the outer lapply is for each element (or repondet) of the nested list of all_tfn_matrices
# it will put each element of the nested list in the "function(tfn_matrices_list)"
# the function, then, put each element of the respondent in the transform_matrix function
# the transform_matrix function is used only to have a single list not nested
transformed_tfn_matrices <- lapply(all_tfn_matrices, function(tfn_matrices_list) {
  lapply(tfn_matrices_list, transform_matrix)
})

# Normalized fuzzy direct relation matrix
print(transformed_tfn_matrices[[1]][[5]]) 

#--------------------

#calculating left score and right score

# Function to create the new 5x2 matrix from a transformed 5x3 matrix
create_5x2_matrix <- function(transformed_tfn_matrix) {
  new_matrix <- matrix(NA, nrow = 5, ncol = 2)
  for (i in 1:5) {
    new_matrix[i, 1] <- transformed_tfn_matrix[i, 2] / (1 + transformed_tfn_matrix[i, 2] - transformed_tfn_matrix[i, 1])
    new_matrix[i, 2] <- transformed_tfn_matrix[i, 3] / (1 + transformed_tfn_matrix[i, 3] - transformed_tfn_matrix[i, 2])
  }
  return(new_matrix)
}

# Apply the creation of the 5x2 matrices to all transformed TFN matrices
new_5x2_matrices <- lapply(transformed_tfn_matrices, function(tfn_matrices_list) {
  lapply(tfn_matrices_list, create_5x2_matrix)
})

# Calculating the left score (ls) and right score (rs) of the normalized matrix
print(new_5x2_matrices[[1]][[5]])

#---------------------

# Evaluation the crisp values

# Function to create the new 5x1 matrix from a new 5x2 matrix
create_5x1_matrix <- function(new_5x2_matrix) {
  new_matrix <- matrix(NA, nrow = 5, ncol = 1)
  for (i in 1:5) {
    a <- new_5x2_matrix[i, 1]
    b <- new_5x2_matrix[i, 2]
    new_matrix[i, 1] <- ((a * (1 - a)) + (b * b)) / (1 - a + b)
  }
  return(new_matrix)
}

# Apply the creation of the 5x1 matrices to all new 5x2 matrices
new_5x1_matrices <- lapply(new_5x2_matrices, function(tfn_matrices_list) {
  lapply(tfn_matrices_list, create_5x1_matrix)
})

# Evaluation of the crisp values
print(new_5x1_matrices[[1]][[5]])

#------------------------

# Computing the normalized crisp value

# Function to create the final 5x1 matrix
create_final_5x1_matrix <- function(tfn_matrix, new_5x1_matrix) {
  min_val <- min(tfn_matrix[, 1])
  max_val <- max(tfn_matrix[, 3])
  final_matrix <- matrix(NA, nrow = 5, ncol = 1)
  for (i in 1:5) {
    final_matrix[i, 1] <- min_val + (new_5x1_matrix[i, 1] * (max_val - min_val))
  }
  return(final_matrix)
}

# Apply the creation of the final 5x1 matrices to all matrices
final_5x1_matrices <- mapply(function(tfn_matrices_list, new_5x1_list) {
  mapply(create_final_5x1_matrix, tfn_matrices_list, new_5x1_list, SIMPLIFY = FALSE)
}, all_tfn_matrices, new_5x1_matrices, SIMPLIFY = FALSE)

# Computing the normalized crisp value
print(final_5x1_matrices[[1]][[5]]) 




# end of fuzzy--------------------


#aggregating the normalized crisp values from all experts and all factors

# Function to sum up the final 5x1 matrices
sum_final_5x1_matrices <- function(final_matrices_list) {
  # Initialize a 5x1 matrix with zeros
  sum_matrix <- matrix(0, nrow = 5, ncol = 1)
  
  # Sum the corresponding elements
  for (mat in final_matrices_list) {
    sum_matrix <- sum_matrix + mat
  }
  
  return(sum_matrix)
}

# Sum all the final 5x1 matrices for each barrier
sum_matrices <- vector("list", length = 5)
#here i is column
for (i in 1:5) {
  sum_matrices[[i]] <- sum_final_5x1_matrices(lapply(final_5x1_matrices, `[[`, i)) #this collects the i-th 5x1 matrix from the list of matrices final_5x1_matrices across all experts
}

# Calculate the average to get the final 5x5 matrix
final_5x5_matrix <- matrix(NA, nrow = 5, ncol = 5)
#here i is column
for (i in 1:5) {
  final_5x5_matrix[, i] <- sum_matrices[[i]] / 36
}

# Total direct-relation matrix
print(final_5x5_matrix)

#-------------------

# the normalized total direct-relation matrix

#Calculate the sum of each row in the final 5x5 matrix
row_sums <- rowSums(final_5x5_matrix)

# Find the maximum value among the row sums
max_row_sum <- max(row_sums)

# Normalize each cell in the final 5x5 matrix by the maximum row sum
normalized_5x5_matrix <- final_5x5_matrix / max_row_sum

# Normalized total direct-relation matrix 
print(normalized_5x5_matrix)

#-------------------

# Create a 5x5 unit (identity) matrix
unit_matrix <- diag(1, 5, 5)
print(unit_matrix)

# Create the resulting 5x5 matrix by subtracting normalized_5x5_matrix from unit_matrix
resulting_5x5_matrix <- unit_matrix - normalized_5x5_matrix

# Print the resulting 5x5 matrix for verification
print(resulting_5x5_matrix)

#-------------------

# capture the indirect influences

# Check if the matrix is invertible by calculating its determinant
det_resulting_5x5 <- det(resulting_5x5_matrix)
print(det_resulting_5x5)

# Invert the resulting 5x5 matrix if it is invertible
if (det_resulting_5x5 != 0) {
  invert <- solve(resulting_5x5_matrix)
  # Print the inverted matrix for verification
  print(invert)
} else {
  print("The resulting 5x5 matrix is not invertible.")
}

#-----------------

# capture both direct and indirect influences

# Perform matrix multiplication
mmult_result <- normalized_5x5_matrix %*% invert

# Total-relation matrix before threshold 
print(mmult_result)

#---------------------

# calculate the D cause degree

# Calculate the sum of each row
row_sums <- rowSums(mmult_result)

# Convert the resulting vector to a 5x1 matrix
D <- matrix(row_sums, nrow = 5, ncol = 1)

# Print the 5x1 matrix D for verification
print(D)

# calculate the R effect degree

# Calculate the sum of each column
col_sums <- colSums(mmult_result)

# Convert the resulting vector to a 5x1 matrix
R <- matrix(col_sums, nrow = 5, ncol = 1)

# Print the 5x1 matrix R for verification
print(R)

#--------------------

# calculate the centrality and net effect

# Compute the element-wise addition
D_plus_R <- D + R

# Compute the element-wise subtraction
D_minus_R <- D - R

# Print the resulting matrices for verification
print("D+R:")
print(D_plus_R)

print("D-R:")
print(D_minus_R)



#---------------------

# calculate the cause-effect-matrix after threshold being set

# Flatten the matrix to a vector to ensure all elements are considered
mmult_vector <- as.vector(mmult_result)
# Calculate the average of all cells in mmult_result
average_value <- mean(mmult_result)

# Print the average value for verification
print(average_value)

# Create the effect_matrix_1 based on the condition
effect_matrix_1 <- ifelse(mmult_result > average_value, mmult_result, 0)

# Print the effect_matrix_1 for verification
print(effect_matrix_1)

#----------------------


# Calculate the minimum value of effect_matrix_1
min_value <- min(effect_matrix_1[effect_matrix_1 != 0])

# Calculate the average value of effect_matrix_1
average_value <- mean(effect_matrix_1[effect_matrix_1 != 0])

# Calculate the maximum value of effect_matrix_1
max_value <- max(effect_matrix_1)

# Calculate the mid-point between average and maximum values
mid_value <- (average_value + max_value) / 2

# Print the calculated values for verification
print(min_value)
print(average_value)
print(max_value)
print(mid_value)

# Create the effect_matrix_2 based on the conditions
effect_matrix_2 <- matrix(NA, nrow = 5, ncol = 5)

for (i in 1:nrow(effect_matrix_1)) {
  for (j in 1:ncol(effect_matrix_1)) {
    cell_value <- effect_matrix_1[i, j]
    if (cell_value >= min_value & cell_value < average_value) {
      effect_matrix_2[i, j] <- "weak"
    } else if (cell_value >= average_value & cell_value < mid_value) {
      effect_matrix_2[i, j] <- "medium"
    } else if (cell_value >= mid_value & cell_value <= max_value) {
      effect_matrix_2[i, j] <- "strong"
    } else {
      effect_matrix_2[i, j] <- NA  # Cells that are not within any specified range
    }
  }
}

# Print the effect_matrix_2 for verification
print(effect_matrix_2)

#------------------

# Create a data frame for plotting
barrier_names <- c("technical", "financial", "knowledge", "environmental", "legal")
D_plus_R_values <- as.vector(D_plus_R)
D_minus_R_values <- as.vector(D_minus_R)

data <- data.frame(
  Barrier = barrier_names,
  D_plus_R = D_plus_R_values,
  D_minus_R = D_minus_R_values,
  Group = ifelse(D_minus_R_values < 0, "Effect Group", "Cause Group")
)

# Print the data for verification
print(data)

# Load necessary library
library(ggplot2)

# Create the plot
plot <- ggplot(data, aes(x = D_plus_R, y = D_minus_R, label = Barrier)) +
  geom_point(aes(color = Group), size = 3) +  # Add points with color based on group
  geom_text(vjust = -1, hjust = 0.5) +        # Add barrier names as labels
  scale_color_manual(values = c("Effect Group" = "red", "Cause Group" = "blue")) +
  labs(title = "Dotted Plot of D+R vs D-R",
       x = "D+R",
       y = "D-R",
       color = "Group") +
  theme_minimal()

# Print the plot
print(plot)


#------------------------


library(igraph)

# Assuming effect_matrix_2 has already been calculated
# Create an edge list from effect_matrix_2
edge_list <- c()

for (i in 1:nrow(effect_matrix_2)) {
  for (j in 1:ncol(effect_matrix_2)) {
    if (!is.na(effect_matrix_2[i, j])) {
      edge_list <- rbind(edge_list, c(i, j, effect_matrix_2[i, j]))  # Correct the direction
    }
  }
}

# Convert to a data frame for easy handling
edge_df <- as.data.frame(edge_list, stringsAsFactors = FALSE)
colnames(edge_df) <- c("from", "to", "level")

# Print the edge data frame for verification
print(edge_df)

# Create the graph from the edge list
graph <- graph_from_data_frame(edge_df, directed = TRUE)



# Define colors based on the connection levels
edge_colors <- ifelse(edge_df$level == "weak", "green",
                      ifelse(edge_df$level == "medium", "blue", "red"))

# Set edge attributes
E(graph)$color <- edge_colors

# Print the graph for verification
print(graph)

# Plot the graph with colored arrows and updated vertex labels
plot(graph,
     vertex.label = V(graph)$name,
     edge.arrow.size = 0.5,
     edge.color = E(graph)$color,
     main = "Directed Graph of Barriers")

#-------------------

print(matrices[[33]])
