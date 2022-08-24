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