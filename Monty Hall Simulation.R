cat("\014") 
##----Import Libraries-----

library(MASS)
require(logging)

# Set logging information
basicConfig()
addHandler(writeToFile, logger="data_logger", file="file_log.log")

monty_hall <- function(strategy = 'STAY'){
  #create a vector with 3 prizes
  prizes = c('goat','goat','car')
  
  #assign the prizes to doors in random order
  doors = sample(prizes,3)
  
  #contestant makes a random choice
  contestant_choice = sample(doors,1)
  
  #remove the contestant's choice from the doors vector
  con_choice = match(contestant_choice,doors)
  doors = doors[-con_choice]
  
  #the host picks a door with the goat
  host_choice = "goat"
  
  #remove the host_choice from the doors
  h_Choice = match(host_choice,doors)
  doors = doors[-h_Choice]
  last_door_outcome = doors
  outcomes <- vector()  
  
  if (strategy == 'STAY' && contestant_choice == 'car') {outcomes <- append(outcomes,1,after=length(outcomes))}
  else if (strategy == 'SWITCH' && last_door_outcome == 'car') {outcomes <- append(outcomes , 1, after = length(outcomes))}
  else {outcomes <- append(outcomes , 0, after = length(outcomes))}    
  return(outcomes)
}



func_main <- function(num_of_simulations = 100,strategy = 'SWITCH'){
  simulation_result <- replicate(num_of_simulations, monty_hall(strategy))
  total_possible_outcomes = length(simulation_result)
  num_of_wins = sum(simulation_result)
  num_of_loss = total_possible_outcomes - num_of_wins
  probabilityofWinning = fractions(num_of_wins/total_possible_outcomes)
  mean_result = mean(simulation_result)
  stddev = sd(simulation_result)
  variance = var(simulation_result)
  return_list = list("probabilityofWinning" = probabilityofWinning ,"variance"=variance,"strategy" = strategy)
  return (return_list)
}


outcome_list = func_main(1000,'STAY')

loginfo((paste('The probability of winning for ',outcome_list[3],' is',outcome_list[1],' and the variance is ',outcome_list[2])),
        logger="data_logger")





