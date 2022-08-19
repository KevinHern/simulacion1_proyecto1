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


simulation_queue_size <- function(day, servers=3) {
  # Defining constants
  mean_attendance <- 8
  stdev_attendance <- 5
  
  arrival_times <- c(0, 1, 2, 3, 4, 5, 6)
  arrival_probabilities <- day_probabilities(day=day)
  
  total_minutes <- 480 # 8 hours shift per day
  
  # Pivot variables
  minute <- 1
  
  # Generating Data Frame
  dataset <- data.frame(matrix(ncol = 6, nrow = 0))
  colnames(dataset) <- c("minute", "servers_available", "dispatching_time", "in_waiting_queue", "new_arrival", "next_arrival")
  
  # Simulation variables
  waiting <- 0
  arrival_queue <- c()
  
  sim_servers <- simservers(available = rep_len(T, servers), timers=rep_len(0, servers))
  
  # Running initial conditions
  first_arrival <- sample(arrival_times, 1, prob = arrival_probabilities)
  print("FIRST ARRIVAL:")
  print(first_arrival)
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
    dataset[nrow(dataset) + 1, ] = c(
      minute, 
      sum(sim_servers@available == T),
      paste(sim_servers@timers, collapse=" "),
      waiting,
      new_arrivals_to_queue, 
      next_arrival + minute
    )
    
    # Increasing timer
    minute <- minute + 1
  }
  
  return(dataset)
}


simulation_queue_size(day = 1, servers = 3)
