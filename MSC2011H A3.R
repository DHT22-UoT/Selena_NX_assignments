# MSC2011H A3
# Ning Xu

setwd("/Users/dannima/Desktop/MBiotech/MSC2011/Exercise/A3_Selena_NX")
#' Prepare a dictionary of words to choose from and save it in a txt file (one column) (the file is called "my_dictionary.txt", and the words are separated by a return)
#' Read the word list from your program.

#' Function read.myword(file, header = FALSE, sep = "\r", ...)
#' ------------------------------------------------------------
#' Reads the word list from file in which words are separated by "\r" 
#' @param : file name (character)
#' @output: data.frame containing the words
read.myword <- function(file, header = FALSE, sep = "\r", ...) {
  read.table(file = file, header = header, sep = sep, ...)
} 

word_list <- read.myword("my_dictionary.txt")

#' Choose a random element from the list. Hint: You may want to check sample() and sample.int()  functions. 

#' Function choose_word(your_word_list)
#' ------------------------------------------------------------
#' Choose a random element from your_word_list and inform the user on the length of the secret word
#' @param : data.frame
#' @output: chooses a random element from the list and prints the length of the element
choose_word <- function(your_word_list) {
  current_word <- sample(1:nrow(your_word_list), 1)
  chosen <- your_word_list[current_word, ]
  #' Inform the user on the length of the secret word. Hint: You may test nchar()
  print(paste("The length of the secret word is", nchar(chosen)))
  return(chosen)
}

#' Ask for user input. The user is expected to enter one character only, check for this.

#' Function ask_input()
#' ------------------------------------------------------------
#' Reads from the user, checks if the user only enters one character and returns the lowercase version of the character.
#' @param : (NULL)
#' @output: a character
ask_input <- function() {
  repeat {
    the_input <- readline(prompt = "Please enter one character that you believe is in the secret word: ")
    if (is.na(the_input) | !(sum(grepl("[a-zA-Z]", the_input)) > 0) | nchar(the_input) != 1) {
      print("Invalid input. Please retry.")
    } else {
      break
    }
  }
  return(tolower(the_input)) #tolower helps to return the lowercase version of the_input 
}

#' Inform the user about the number of wrong guesses/tries allowed. You decide on the rule here and implement it. Please comment your code appropriately.
#number of mistakes
mistakes <- 3
print(paste("The number of wrong guesses allowed is", mistakes))

#' Check to see if the user input is in the secret word.
#' If yes, notify user and ask for next letter
#' If not, notify user.
#' If user has not exhausted the available tries, ask for the next letter.
secret_word <- choose_word(word_list)
secret_char <- strsplit(secret_word, "")
indices <- c()
clue <- rep("_", nchar(secret_word)) # Edit made by Danni: set up visual clue by replicating "_" to the length of the secret_word.

while (mistakes > 0) {
  user_input <- ask_input()
  if(sum(grepl(user_input, secret_char[[1]], ignore.case = T)) > 0) { #check to see if user_input is in the secret word
    for(i in seq_along(secret_char[[1]])) {
      if (user_input == secret_char[[1]][i]) {
        indices <- append(indices, -i)
      } 
    }
    secret_char[[1]] <- secret_char[[1]][indices]  #if user_input is in the secret word, I will remove the letter(s) from the secret word
    indices <- c()
    
    # Edit made by Danni: Update & display the visual clue
    ind <- unlist(gregexpr(user_input, secret_word)) # DM: collect the index/indices of the correctly guess letter
    clue <- replace(clue, ind, user_input) # DM: replace "_" with correctly guessed letter at the corresponding indices
    cat(clue, sep = " ") # DM: concatenate the clue for display
    
    if(length(secret_char[[1]]) == 0) { #the user has guessed all letters correctly
      mistakes <- -1 #so that we can exit the while loop
      # If user has guessed the whole word correctly, notify user that they’ve won. Reveal secret and exit.
      print(paste("You won! You have guessed the word correctly, which is:", secret_word))
    } else {
      # If user has not exhausted the available tries, ask for the next letter.
      print("Correct! Please guess another character.")
    }
  } else {
    mistakes <- mistakes - 1 #If user has guessed incorrectly or entered a repeated letter, the number of wrong guesses allowed is reduced by 1.
    print(paste("Incorrect or you may have entered a repeated letter. Please guess another character. Your number of wrong guesses allowed is now", mistakes))
  }
}
#' If tries are exhausted, notify user that they’ve lost. Reveal secret and exit.
if(mistakes == 0) {
  print(paste0("Sorry, you have lost. The secret word is: ", secret_word, ". Exiting game..."))
  #' Edit made by Danni: When using the paste function, there is an extra space after the secret_word. 
  #' Suggestion would be to use paste0 and add in an extra space after "The secret word is:".
}

#' Comment by Danni: Hi Selena! Your code followed a consistent and organized 
#' structure with detailed comments. I really like how you created helper functions 
#' for reading the txt file and generating the random word. After running the 
#' code, the code worked as anticipated with no error messages. In terms of the
#' functionality of the game, I think it would be really helpful to provide the 
#' users with some visual cues, so the users are aware of the number of characters 
#' correctly guessed. I added some lines of how I made my visual clue with few
#' comments on the side. Other than that, great work overall!


