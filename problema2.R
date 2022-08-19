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


simulation_day_time_limit <- function(day, servers=3) {
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
  #   "total_attended",
  #   "total_waiting_time",
  #   "in_queue",
  #   "next_arrival"
  # )
  # 
  # dataset <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # colnames(dataset) <- column_names
  
  # Simulation variables
  waiting_queue <- c()
  waiting_queue_timers <- c()
  arrival_queue <- c()
  
  total_waiting_time <- 0
  total_attended <- 0
  
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
    
    # Increase waiting queue timers
    if(length(waiting_queue_timers) > 0) {
      waiting_queue_timers <- sapply(waiting_queue_timers, increase_timer)
    }
    
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
        
        # Adding the person to arrival_queue
        arrival_queue <- append(arrival_queue, next_arrival)
        
        if(next_arrival > 0) {
          break
        }
      }
    }
    
    ### ARRIVING AND WAITING
    #print("ARRIVING AND WAITING")
    
    # Checking how many are arriving at the moment
    total_arriving <- length(arrival_queue[arrival_queue == 0])
    #print(total_arriving)
    
    # Determining the rest of the arrival queue (who are yet to come)
    arrival_queue <- arrival_queue[arrival_queue > 0]
    
    # Add to waiting queue and adding a timer
    waiting_queue <- append(waiting_queue, rep_len(T, total_arriving))
    waiting_queue_timers <- append(waiting_queue_timers, rep_len(0, total_arriving))
    
    #print(waiting_queue_timers)
    
    ### DISPATCHING
    #print("DISPATCHING")
    
    # Before dispatching, check servers availability and how many are waiting
    # Only dispatch if there is at least one server available
    while(server_available(sim_servers = sim_servers) && length(waiting_queue) > 0) {
      # Dispatching people
      sim_servers <- dispatch(sim_servers = sim_servers)
      
      # Pop waiting_queue and increased the total attended people
      total_attended <- total_attended + 1
      waiting_queue <- waiting_queue[-1]
      
      # Pop waiting_queue_timers and adding the first most waiting time to the total
      # waiting time counter
      total_waiting_time <- total_waiting_time + waiting_queue_timers[1]
      waiting_queue_timers <- waiting_queue_timers[-1]
    }
    
    #Filling dataframe
    # dataset[nrow(dataset) + 1, ] = c(
    #   minute,
    #   sum(sim_servers@available == T),
    #   paste(sim_servers@timers, collapse=" "),
    #   total_attended,
    #   total_waiting_time,
    #   length(waiting_queue),
    #   next_arrival + minute
    # )
    
    # Increasing timer
    minute <- minute + 1
  }
  
  return(c(total_waiting_time, total_attended))
}

# View(simulation_day_time_limit(day=1, servers =3))

simulate_week <- function(index, server=3){
  # Initializing constants
  total_days <- 7
  
  days <- c(1:total_days)
  
  # Running simulation
  week_summary <- sapply(days,
                        simulation_day_time_limit,
                        servers=server
  )
  
  # Calculating weekly stats
  total_week_waiting_time <- sum(week_summary[1, ])
  total_week_attended <- sum(week_summary[2, ])
  
  # Returning weekly summary
  return(c(total_week_waiting_time, total_week_attended))
}

#View(simulate_week(server=2))

simulation_problem_two <- function(nsim, time_limit) {
  # Initializing constants
  simulations <- c(1:nsim)
  
  # Initializing pivot variables
  servers <- 1
  
  # Creating dataframe
  column_names <- c(
    "servers",
    "total_waiting_time",
    "total_attended",
    "average_waiting_time"
  )

  dataset <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(dataset) <- column_names
  
  # Running simulations
  repeat{
    # Running simulation with N servers
    simulation_results_with_given_server_number <- sapply(
      simulations,
      simulate_week,
      server=servers
    )
    
    simulation_results_with_given_server_number <- t(simulation_results_with_given_server_number)
    
    # Calculating average waiting time after the simulation
    total_simulation_waiting_time <- sum(simulation_results_with_given_server_number[ , 1])
    total_simulation_attended <- sum(simulation_results_with_given_server_number[ , 2])
    
    simulation_average_waiting_time <- total_simulation_waiting_time / total_simulation_attended
    
    #Filling dataframe
    dataset[nrow(dataset) + 1, ] = c(
      servers,
      total_simulation_waiting_time,
      total_simulation_attended,
      simulation_average_waiting_time
    )
    
    # if(servers == 7) {
    #   return(dataset)
    # }
    # 
    # servers <- servers + 1
    
    # Compare simulation average waiting time with the time limit
    if(simulation_average_waiting_time >= time_limit) {
      # If the simulation time exceeds the time limit, increase number of servers by 1
      # and redo simulation
      servers <- servers + 1
    } else {
      # If the simulation time is lower than the time limit, then return the number
      # of servers and the average waiting time
      return(dataset)
    }
  }
}

View(simulation_problem_two(nsim = 10, time_limit=15))
