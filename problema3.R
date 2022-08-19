day_probabilities <- function(day){
  probabilities <- c()
  if(day == 1) {
    # Monday
    probabilities <- append(probabilities, c(0.10, 0.15, 0.10, 0.35, 0.25, 0.05, 0))
  } else if(day == 2) {
    # Tuesday
    probabilities <- append(probabilities, c(0.10, 0.10, 0.15, 0.20, 0.35, 0.10, 0))
  } else if(day == 3) {
    # Wednesday
    probabilities <- append(probabilities, c(0, 0.10, 0.10, 0.20, 0.10, 0.25, 0.25))
  } else if(day == 4) {
    # Thursday
    probabilities <- append(probabilities, c(0, 0.15, 0.20, 0.20, 0.15, 0.15, 0.15))
  } else if(day == 5) {
    # Friday
    probabilities <- append(probabilities, c(0.15, 0.15, 0.20, 0.20, 0.10, 0.10, 0.10))
  } else if(day == 6) {
    # Saturday
    probabilities <- append(probabilities, c(0.20, 0.15, 0.10, 0.50, 0.05, 0, 0))
  } else {
    # Sunday
    probabilities <- append(probabilities, c(0.35, 025, 0.20, 0.10, 0.10, 0, 0))
  }
  return(probabilities)
}

server_available <- function(sim_servers) {
  return(!is.na(match(T, sim_servers@available)))
}

decrease_timer <-function(timer) {
  if(timer > 0) {
    return(timer-1)
  } else {
    return(0)
  }
}

increase_timer <- function(timer) {
  return(timer+1)
}

liberate_server <- function(server_timer) {
  if(server_timer == 0) {
    return(T)
  } else {
    return(F)
  }
}

dispatch <- function(sim_servers, mean_arrival=8, stdev_arrival=5) {
  server_available_index <- match(T, sim_servers@available)
  
  if(is.na(server_available_index)) {
    return(NA)
  } else {
    sim_servers@available[server_available_index] <- F
    
    attendance_time <- floor(rnorm(1, mean_arrival, stdev_arrival))
    
    if(attendance_time  < 1) {
      attendance_time <- 1
    }
    
    sim_servers@timers[server_available_index] <- attendance_time
    return(sim_servers)
  }
}

# Creating Servers Object
simservers <- setClass("simservers", slots=c(available="vector", timers="vector"))

simulation_day <- function(day, servers=3, queue_limit=10) {
  # Defining constants
  mean_attendance <- 8
  stdev_attendance <- 5
  
  arrival_times <- c(0, 1, 2, 3, 4, 5, 6)
  arrival_probabilities <- day_probabilities(day=day)
  
  total_minutes <- 480 # 8 hours shift per day
  
  # Pivot variables
  minute <- 1
  
  # Generating Data Frame
  # column_names <- c(
  #   "minute",
  #   "servers_available",
  #   "dispatching_time",
  #   "in_queue",
  #   "did_not_get_queued",
  #   "next_arrival"
  # )
  # 
  # dataset <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # colnames(dataset) <- column_names
  
  # Simulation variables
  waiting_queue <- 0
  arrival_queue <- c()
  total_people_went_away <- 0
  
  sim_servers <- simservers(available = rep_len(T, servers), timers=rep_len(0, servers))
  
  # Running initial conditions
  first_arrival <- sample(arrival_times, 1, prob = arrival_probabilities)
  # print("FIRST ARRIVAL:")
  # print(first_arrival)
  arrival_queue <- append(arrival_queue, first_arrival)
  
  # Running simulation
  while(minute<=total_minutes) {
    
    ### TIMERS MANIPULATION
    #print("TIMERS MANIPULATION")
    
    # Decrease arrival_queue timers
    arrival_queue <- sapply(arrival_queue, decrease_timer)
    
    # Decrease Servers attendance timers
    sim_servers@timers <- sapply(sim_servers@timers, decrease_timer)
    
    ### LIBERATING SERVERS
    #print("LIBERATING SERVERS")
    
    # Once a server timer has reached 0, make it available
    sim_servers@available <- sapply(sim_servers@timers, liberate_server)
    
    ### DETERMINING NEXT ARRIVAL
    #print("DETERMINING NEXT ARRIVAL")
    
    # Check if a person just arrived, if so, determine when the next one will arrive
    # Ignore otherwise
    next_arrival <- NA
    if(0 %in% arrival_queue){
      repeat{
        # Determining when is the next person arriving
        next_arrival <- sample(arrival_times, 1, prob = arrival_probabilities)
        
        arrival_queue <- append(arrival_queue, next_arrival)
        
        if(next_arrival > 0) {
          break
        }
      }
    }
    
    ### ARRIVING AND WAITING
    #print("ARRIVING AND WAITING")
    
    # Increasing the waiting counter if some counters in arrival_queue have reached 0
    new_arrival_queue <- arrival_queue[arrival_queue > 0]
    
    new_arrivals_to_queue <- length(arrival_queue) - length(new_arrival_queue)
    
    # Checking if its possible to queue people by observing the queue limit
    servers_just_got_liberated <- match(T, sim_servers@available)
    if(is.na(servers_just_got_liberated)) {
      servers_just_got_liberated <- 0
    } else {
      servers_just_got_liberated <- length(servers_just_got_liberated)
    }
    possible_to_queue <- (queue_limit - waiting_queue) + servers_just_got_liberated
    
    if(possible_to_queue > 0) {
      # Determine how many can be queued
      if(new_arrivals_to_queue >= possible_to_queue){
        waiting_queue <- waiting_queue + possible_to_queue
        total_people_went_away <- total_people_went_away + (new_arrivals_to_queue - possible_to_queue)
      } else {
        waiting_queue <- waiting_queue + new_arrivals_to_queue
      }
    } else {
      # Increase the counter of how many people did not get queued
      total_people_went_away <- total_people_went_away + new_arrivals_to_queue
    }
    
    arrival_queue <- new_arrival_queue
    
    ### DISPATCHING
    #print("DISPATCHING")
    
    # Before dispatching, check servers availability and how many are waiting
    # Only dispatch if there is at least one server available
    while(server_available(sim_servers = sim_servers) && waiting_queue > 0) {
      sim_servers <- dispatch(sim_servers = sim_servers)
      waiting_queue <- waiting_queue - 1
    }
    
    # Filling dataframe
    # dataset[nrow(dataset) + 1, ] = c(
    #   minute,
    #   sum(sim_servers@available == T),
    #   paste(sim_servers@timers, collapse=" "),
    #   waiting_queue,
    #   total_people_went_away,
    #   next_arrival + minute
    # )
    
    # Increasing timer
    minute <- minute + 1
  }
  
  return(total_people_went_away)
}

#View(simulation_day(day = 1, servers = 1, queue_limit = 10))

simulate_week <- function(index, server=3, queue_limit=10){
  # Initializing constants
  total_days <- 7
  
  days <- c(1:total_days)
  
  # Running simulation
  week_summary <- sapply(days,
                         simulation_day,
                         servers=server,
                         queue_limit=queue_limit
  )
  
  # Calculating weekly stats
  total_week_not_queued <- sum(week_summary)
  
  # Returning weekly summary
  return(total_week_not_queued)
}

# View(simulate_week(1, server = 2, queue_limit = 30))

simulation_problem_three <- function(nsim, servers, queue_limit) {
  # Initializing constants
  simulations <- c(1:nsim)
  
  # Initializing pivot variables
  no_servers <- 1
  
  # Creating dataframe
  column_names <- c(
    "servers",
    "total_not_queued",
    "days",
    "average_not_queued"
  )
  
  dataset <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(dataset) <- column_names
  
  # Running simulations
  repeat{
    simulation_results <- sapply(
      simulations,
      simulate_week,
      server=no_servers,
      queue_limit=queue_limit
    )
    
    # Calculating average person that left per day
    total_people_not_got_queued <- sum(simulation_results)
    total_days <- 7*nsim
    average_people_not_queued <- total_people_not_got_queued/total_days
    
    #Filling dataframe
    dataset[nrow(dataset) + 1, ] = c(
      no_servers,
      total_people_not_got_queued,
      total_days,
      average_people_not_queued
    )
    
    
    # Keep simulating until the number of servers is reached
    if(no_servers == servers) {
      # If the simulation time exceeds the time limit, increase number of servers by 1
      # and redo simulation
      return(dataset)
    }
    
    # Increasing Servers number
    no_servers <- no_servers + 1
  }
  return(matrix(c(total_people_not_got_queued, total_days, average_people_not_queued), ncol = 3))
  
}

View(simulation_problem_three(nsim = 20, servers=5, queue_limit=20))