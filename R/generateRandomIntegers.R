# 
# generateRandomIntegers <- function(total, parts) {
#   if(parts>1){
#     # Generate initial random integers
#     random_integers <- sample(1:(total - 1), parts - 1, replace = FALSE)
#     random_integers <- sort(random_integers)
#     
#     # Calculate the differences to ensure they sum to total
#     differences <- c(random_integers[1],
#                      diff(random_integers),
#                      total - random_integers[length(random_integers)])
#   }
#   if(parts==1){
#     differences <- total
#   }
#   return(differences)
# }
