## Simulation
# simulate one realization of an outbreak in a single household
# Input: Household size n and immune parameters qc & qh
# Output: The total number of people who eventually got sick (total_infected)

# Defining the Model Parameters
# n (Household Size): How many people live in this house
# "q" stands for the probability of avoiding (escaping) infection
# qh (Household Escape Probability): The probability a susceptible person avoids infection from one infected household member.
# qc (Community Escape Probability): The probability a susceptible person avoids infection from the community.
# max_gen: The maximum number of "generations" (waves of infection) allowed.

# Initialization
# S (Susceptible): We start assuming everyone in the house is healthy but vulnerable (n people)
# I (Infected): Initially, zero people are infected inside the house
# total_infected: A counter to track the final outbreak size.

simulate_household <- function(n, qh, qc, max_gen = 10) {
  S <- n
  I <- 0
  total_infected <- 0
  
  # Generation 0: Infection enters from the community
  # Logic: The infection must start somewhere.
  # This first step simulates household members catching the virus from the "community" (outside world).
  # Since qc = Probability of escaping infection from the community
  # 1−qc = Probability of getting infected from the community
  
  # 1: We are running this experiment 1 time for this household.
  # S: The number of trials (people in the house).
  # 1 - qc: The probability of "success" (getting infected) for each person.
  # initial_cases: The number of people who brought the virus home
  initial_cases <- rbinom(1, S, 1 - qc)
  I <- initial_cases
  S <- S - initial_cases # We move the newly infected people from S (Healthy) to I (Infected).
  total_infected <- total_infected + initial_cases
  
  # Generation 1+ (Household Transmission Loop)
  # The simulation continues as long as: there are infectious people (I > 0) & there are still healthy people to infect (S > 0)
  # Assume the simulation continues in discrete generations
  # assume people infected in generation t are infectious only for that generation
  # In generation t+1, they are recovered, and the newly infected people become the spreaders
  gen <- 1
  while (gen <= max_gen && I > 0 && S > 0) {
    # Generation 1+: Infection spreads inside the house based on how many people are already sick
    # Probability of escaping infection from household contacts: qh^I
    # Logic: You are a healthy person. There are I sick people in the house. 
    # Infection attempts are independent. To stay safe, you must escape Person 1 & Person 2 & ...
    # p_escape_house = qh * qh * qh.....
    p_escape_house <- qh^I
    # We assume community infection is the initial seed (Generation 0)
    # Subsequent generations are driven by household transmission
    # p_escape_total: The total chance a susceptible person survives this wave without getting sick
    # p_infect: The probability they fail to escape (get sick)
    p_escape_total <- p_escape_house 
    p_infect <- 1 - p_escape_total
    # Simulating New Infections
    # S: remaining healthy people
    new_inf <- rbinom(1, S, p_infect) 
    I <- new_inf # The newly sick become the infectors for the next round
    S <- S - new_inf # Remove them from the healthy pool
    total_infected <- total_infected + new_inf # Add to total count
    gen <- gen + 1 # Advance time
  }
  return(total_infected)
}

# We use this function in ABC Algorithm to see if a specific pair of (qc,qh) produces "Total Infected" counts that match the real history. 
# If the simulation matches the data, the parameters are accepted as plausible.