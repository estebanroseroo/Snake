# FIND THE LONGEST SNAKE

# Function to find the longest snake
findLongestSnake <- function(matrix){
  print(matrix);
  if(!is.matrix(matrix) | !is.numeric(matrix)) 
    stop("Numeric matrix is expected.");
  
  longestSnake <- NULL;
  for(r in 1:nrow(matrix)){
    for(c in 1:ncol(matrix)){
      snake <- move(r,c,matrix);
      
      if(length(longestSnake) < length(snake))
        longestSnake <- snake;
    }
  }
  cat("The longest snake: ", longestSnake, "\n");
}

# Function to move the snake right or down
move <- function(row, column, matrix){
  right <- down <- matrix[row,column];
  
  if(column < ncol(matrix)){
    if(matrix[row,column+1] == matrix[row,column]+1 | matrix[row,column+1] == matrix[row,column]-1)
      right <- c(right,move(row, column+1, matrix));
  }
  
  if(row < nrow(matrix)){
    if(matrix[row+1,column] == matrix[row,column]+1 | matrix[row+1,column] == matrix[row,column]-1)
      down <- c(down,move(row+1, column, matrix));
  }
  
  if(length(right)<length(down)){
    return(down);
  }else{
    return(right);
  }
}

findLongestSnake(matrix(sample(1:4, 16, replace = TRUE), nrow = 4))
