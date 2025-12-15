##test for simulate_household to make sure it works##

library(testthat)

#original function
simulate_household <- function(n, qh, qc, max_gen = 10) {
  S <- n
  I <- 0
  total_infected <- 0
  new_comm <- rbinom(1, S, 1 - qc) 
  I <- new_comm
  S <- S - new_comm
  total_infected <- total_infected + new_comm
  gen <- 1
  while (gen <= max_gen && I > 0 && S > 0) {
    p_escape_house <- qh^I
    p_escape_total <- p_escape_house 
    p_infect <- 1 - p_escape_total
    new_inf <- rbinom(1, S, p_infect)
    I <- new_inf
    S <- S - new_inf
    total_infected <- total_infected + new_inf
    gen <- gen + 1
  }
  return(total_infected)
}



## test ##

test_that("simulate_household works", {
            #5 ppl household
            set.seed(1)
            output <- simulate_household(n = 5, qh = 0.5, qc = 0.5)
            #make sure that output is between 0 (no one infected) and 5 (everyone infected)
            expect_true(output >= 0 && output <= 5)
            
            #no household infections (qh = 1)
            set.seed(1)
            output <- simulate_household(n = 5, qh = 1, qc = 0.5)
            expect_true(output >= 0 && output <= 5)
            
            #all residents infected from the community (qc = 0)
            set.seed(1)
            output <- simulate_household(n = 5, qh = 0.5, qc = 0)
            expect_true(output == 5)
            
            #no infections from community (qc = 1)
            set.seed(1)
            output <- simulate_household(n = 5, qh = 0, qc = 1)
            expect_true(output == 0)
          })


