source("utilities.R")

simulation_day_queue_size <- function(day, servers=3) {
  # Defining constants
  mean_attendance <- 8
  stdev_attendance <- 5
  
  arrival_times <- c(0, 1, 2, 3, 4, 5, 6)
  arrival_probabilities <- day_probabilities(day=day)
  
  total_minutes <- 480 # 8 hours shift per day
  
  # Pivot variables
  minute <- 1
  
  # Generating Data Frame
  # dataset <- data.frame(matrix(ncol = 6, nrow = 0))
  # colnames(dataset) <- c("minute", "servers_available", "dispatching_time", "in_waiting_queue", "new_arrival", "next_arrival")
  
  # Simulation variables
  waiting <- 0
  arrival_queue <- c()
  total_waiting <- 0
  
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
    if(!is.na(match(0, arrival_queue))){
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
    
    # Increasing the waiting counter if some counters in arrival_queue have reached 0
    new_arrival_queue <- arrival_queue[arrival_queue > 0]
    
    new_arrivals_to_queue <- length(arrival_queue) - length(new_arrival_queue)
    waiting <- waiting + new_arrivals_to_queue
    
    arrival_queue <- new_arrival_queue
    
    ### DISPATCHING
    #print("DISPATCHING")
    
    # Before dispatching, check servers availability and how many are waiting
    # Only dispatch if there is at least one server available
    while(server_available(sim_servers = sim_servers) && waiting > 0) {
      sim_servers <- dispatch(sim_servers = sim_servers)
      waiting <- waiting - 1
    }
    
    # Filling dataframe
    # dataset[nrow(dataset) + 1, ] = c(
    #   minute, 
    #   sum(sim_servers@available == T),
    #   paste(sim_servers@timers, collapse=" "),
    #   waiting,
    #   new_arrivals_to_queue, 
    #   next_arrival + minute
    # )
    
    # Increasing waiting counter
    total_waiting <- total_waiting + waiting
    
    # Increasing timer
    minute <- minute + 1
  }
  
  return(total_waiting)
}

simulate_week <- function(server){
  # Initializing constants
  total_days <- 7
  
  days <- c(1:total_days)
  
  # Running simulation
  weekly_waiting_queue_size <- sapply(days,
         simulation_day_queue_size,
         servers=server
  )
  
  # Returning week's total queue size
  week_total_queue_size <- sum(weekly_waiting_queue_size)
  
  return(week_total_queue_size)
}

# View(simulate_week(server=1))

simulate_week_server<- function(index) {
  # Initializing constants
  total_servers <- 7
  
  servers <- c(1:total_servers)
  
  # Running Simulation
  weekly_queue_size_per_server <- sapply(servers, simulate_week)
  
  return(weekly_queue_size_per_server)
  
}

# View(simulate_week_server(index=1))

simulation_problem_one <- function(nsim) {
  # Initializing constants
  simulations <- c(1:nsim)
  
  # Running simulations
  weekly_queue_size_per_server_per_simulation <- sapply(simulations, simulate_week_server)
  
  # Reconstructing dataframe
  fixed_dataset <- as.data.frame(t(weekly_queue_size_per_server_per_simulation))
  
  # Calculating Averages
  total_minutes <- 480*7*nsim
  average_one_server <- sum(fixed_dataset$V1)/total_minutes
  average_two_server <- sum(fixed_dataset$V2)/total_minutes
  average_three_server <- sum(fixed_dataset$V3)/total_minutes
  average_four_server <- sum(fixed_dataset$V4)/total_minutes
  average_five_server <- sum(fixed_dataset$V5)/total_minutes
  average_six_server <- sum(fixed_dataset$V6)/total_minutes
  average_seven_server <- sum(fixed_dataset$V7)/total_minutes
  
  # Returning dataframe
  return(c(
    average_one_server,
    average_two_server,
    average_three_server,
    average_four_server,
    average_five_server,
    average_six_server,
    average_seven_server
    )
  )
}

View(simulation_problem_one(nsim = 3))
