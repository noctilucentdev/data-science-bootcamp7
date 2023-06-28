# HW-Create Chatbot

greeting <- function() {
  print("Welcome to My Chatbot.")
  name = readline("What is your name?")
  text = paste("Hello", name)
  print(text)
  age = readline("How old are you?")
  text2 = paste("You are ", age, " years old.")
  print(text2)
  age_num <- as.numeric(age)
  return (age_num)
}

lottery_sample <- function() {
  a <- 0:9
  first_digit = as.character(sample(a, 1))
  second_digit = as.character(sample(a, 1))
  full_digit = paste(first_digit, second_digit, sep='')
  return (as.numeric(full_digit))
}

compare_num <- function(x,y) {
  if (x==y) {
    print("You may get lucky!, You should buy 2-digits based on your age")
  } else {
    print("You should not buy 2-digits based on your age!")
  }
}

buy_lottery_with_your_age <- function() {
  while (TRUE) {
    your_age = greeting()
    print("Do you think you should buy 2-digits lottery in this round based on your age?")
    print("Let's predict the 2-digits prize of this round.")
    predict_digit=lottery_sample()
    print(paste("The 2-digits prize is", predict_digit))
    print(paste("As your age is", your_age, "and predicted 2-digits is", predict_digit))
    compare_num(your_age, predict_digit)
    play_again = readline("Do you want to play again? \n
    type 'y' to play again or type 'n' to stop the chatbot")
    if (play_again == "y") {
      print("Let's play again.")
    } else if (play_again == "n") {
      print("Thank you for playing with us.")
      break
    } else {
      print("You don't type 'y' or 'n' so the chatbot shall stop.")
      print("Thank you for playing with us.")
      break
    }
  }
    
  }

buy_lottery_with_your_age()


# HW - Pao-Ying-Choop

bot=""
choose_hammer <- function(bot){
  if (bot == "s"){
    print("You win.")
  } else if (bot == "p"){
    print("You lose.")
  } else if (bot == "h") {
    print("We tie.")
  }
}

choose_scissors <- function(bot){
  if (bot == "s"){
    print("We tie.")
  } else if (bot == "p"){
    print("You win.")
  } else if (bot == "h") {
    print("You lose.")
  }
}

choose_paper <- function(bot){
  if (bot == "s"){
    print("You lose.")
  } else if (bot == "p"){
    print("We tie.")
  } else if (bot == "h") {
    print("You win.")
  }
}

bot_choose <- function(){
  choices = c("s", "p", "h")
  bot_choice = sample(choices, 1)
  return (bot_choice)
}

hammer_scissors_paper <- function() {
  
  print("Hello. This is hammer-scissors-paper game.")
  name = readline("What is your name?")
  print(paste("Welcome", name, "Nice to have a chance to play with you"))
  writeLines("Instruction : \n
      - Please choose 'Hammer' or 'Scissors' or 'Paper' \n
      - 'Hammer' defeats 'Scissors', 'Scissors' defeats 'Paper' and 'Paper' defeats 'Hammer'")
  while (TRUE) {
    print("What will you choose?")
    your_choice = readline("Type 'h' for 'Hammer', 's' for 'Scissors', 'p' for 'Paper'")
    bot_selection = bot_choose()
    print(paste("I choose", bot_selection))
    if (your_choice=='h'){
      choose_hammer(bot_selection)
    } else if (your_choice=='s') {
      choose_scissors(bot_selection)
    } else if (your_choice=='p') {
      choose_paper(bot_selection)
    } else {
      print("You did not type 'h' or 's' or 'p'.")
      # continue = readline("Do you want to play again? type 'y' or 'n'")
      # if (continue=='y'){
      #   print("OK. Let's play again")
      # } else {
      #   print("Thank you. Chatbot ends now.")
      #   break
      # }
    }
    playagain_hsp = readline("Do you want to play again? type 'y' or 'n'")
    if (playagain_hsp=='y'){
      print("OK. Let's play again")
    } else {
      print("Thank you. Chatbot ends now.")
      break
    }
  }
  
}


