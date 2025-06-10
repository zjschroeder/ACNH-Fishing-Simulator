library(dplyr)
library(data.table)
library(ggplot2)
library(dplyr)
library(patchwork) 

optimize_fish_catching <- function(df, 
                                   N = 10000,
                                   exploration_weight = 0.3,
                                   column_minimization_weight = 0.5) {
  #' Optimized fish catching
  #' 
  #' @param df Data frame with 'name', 'location_collapsed', and time columns
  #' @param N Number of simulation draws to perform
  #' @param exploration_weight Higher values (0-1) encourage exploration
  #' @param column_minimization_weight Higher values (0-1) prioritize fewer columns
  #' @return List with optimization results and statistics
  

  dt <- as.data.table(df)
  
  # Pre-compute  static elements
  time_columns <- names(dt)[!names(dt) %in% c('name', 'location_collapsed')]
  n_time_cols <- length(time_columns)
  fish_names <- dt$name
  n_fish <- length(fish_names)
  locations <- unique(dt$location_collapsed)
  n_locations <- length(locations)
  
  # Create lookup tables
  fish_to_idx <- setNames(seq_len(n_fish), fish_names)
  col_to_idx <- setNames(seq_len(n_time_cols), time_columns)
  
  # Probability matrices
  location_fish_idx <- vector("list", n_locations)
  prob_matrices <- vector("list", n_locations)
  nonzero_masks <- vector("list", n_locations)  
  names(location_fish_idx) <- locations
  names(prob_matrices) <- locations
  names(nonzero_masks) <- locations
  
  for (i in seq_along(locations)) {
    loc <- locations[i]
    loc_rows <- dt$location_collapsed == loc
    location_fish_idx[[i]] <- which(loc_rows)
    prob_matrix <- as.matrix(dt[loc_rows, ..time_columns])
    prob_matrices[[i]] <- prob_matrix
    
    # Create mask so don't waste time on 0% probability fish
    nonzero_masks[[i]] <- prob_matrix > 0
  }
  
  # Empty vectors
  best_score <- Inf
  best_solution <- NULL
  
  # Summary stats
  successful_runs <- 0
  total_draws_sum <- 0
  total_draws_sq_sum <- 0
  columns_used_sum <- 0
  columns_used_sq_sum <- 0
  min_draws <- Inf
  max_draws <- 0
  min_cols <- Inf
  max_cols <- 0
  
  # Simulation parameters
  exploration_threshold <- exploration_weight
  col_penalty <- column_minimization_weight * 0.1
  scale_factor <- 50
  max_attempts <- 1000
  
  # Main simulation
  for (sim in seq_len(N)) {
    if (sim %% 2000 == 0) {
      cat("Simulation", sim, "/", N, 
          "- Success rate:", round(successful_runs/sim * 100, 1), "%\n")
    }
    
    caught_fish <- logical(n_fish)
    columns_used <- logical(n_time_cols)
    total_draws <- 0L
    column_usage_count <- integer(n_time_cols)
    
    attempt <- 0L
    n_caught <- 0L
    
    while (n_caught < n_fish && attempt < max_attempts) {
      attempt <- attempt + 1L
      
      # Column selection
      if (runif(1) < exploration_threshold || sum(columns_used) == 0) {
        if (runif(1) < 0.5) {
          chosen_col_idx <- sample.int(n_time_cols, 1)
        } else {
          if (sum(column_usage_count) > 0) {
            min_usage <- min(column_usage_count)
            candidate_cols <- which(column_usage_count == min_usage)
            chosen_col_idx <- sample(candidate_cols, 1)
          } else {
            chosen_col_idx <- sample.int(n_time_cols, 1)
          }
        }
      } else {
        # Exploitation
        column_scores <- numeric(n_time_cols)
        
        for (loc_idx in seq_along(locations)) {
          uncaught_in_loc <- location_fish_idx[[loc_idx]][!caught_fish[location_fish_idx[[loc_idx]]]]
          
          if (length(uncaught_in_loc) > 0) {
            #  probability calculation
            loc_probs <- prob_matrices[[loc_idx]]
            uncaught_local_idx <- match(uncaught_in_loc, location_fish_idx[[loc_idx]])
            
            # Probability of catching at least one fish per column
            probs_uncaught <- loc_probs[uncaught_local_idx, , drop = FALSE]
            prob_catch_none <- apply(1 - probs_uncaught, 2, prod)
            prob_catch_at_least_one <- 1 - prob_catch_none
            
            column_scores <- column_scores + prob_catch_at_least_one * length(uncaught_in_loc)
          }
        }
        
        # Minimization penalty 
        penalty_multiplier <- ifelse(columns_used, 1 + col_penalty, 1 - col_penalty)
        column_scores <- column_scores * penalty_multiplier
        
        # No negative probabilities and handle edge cases
        column_scores <- pmax(column_scores, 0) 
        
        total_score <- sum(column_scores)
        if (total_score > 0 && !any(is.na(column_scores)) && !any(is.infinite(column_scores))) {
          chosen_col_idx <- sample.int(n_time_cols, 1, prob = column_scores)
        } else {
          chosen_col_idx <- sample.int(n_time_cols, 1)
        }
      }
      
      # Update 
      columns_used[chosen_col_idx] <- TRUE
      column_usage_count[chosen_col_idx] <- column_usage_count[chosen_col_idx] + 1L
      
      # Simulate skip zero probability cases
      new_catches <- 0L
      for (loc_idx in seq_along(locations)) {
        fish_in_loc <- location_fish_idx[[loc_idx]]
        uncaught_in_loc <- fish_in_loc[!caught_fish[fish_in_loc]]
        
        if (length(uncaught_in_loc) > 0) {
          uncaught_local_idx <- match(uncaught_in_loc, fish_in_loc)
          
          # fish with non-zero probability?
          if (any(nonzero_masks[[loc_idx]][uncaught_local_idx, chosen_col_idx])) {
            probs <- prob_matrices[[loc_idx]][uncaught_local_idx, chosen_col_idx]
            
            nonzero_fish <- probs > 0
            if (any(nonzero_fish)) {
              nonzero_probs <- probs[nonzero_fish]
              nonzero_indices <- uncaught_in_loc[nonzero_fish]

              catches <- runif(length(nonzero_probs)) < nonzero_probs
              caught_indices <- nonzero_indices[catches]
              
              if (length(caught_indices) > 0) {
                caught_fish[caught_indices] <- TRUE
                new_catches <- new_catches + length(caught_indices)
              }
            }
          }
        }
      }
      
      total_draws <- total_draws + max(1L, new_catches)
      n_caught <- sum(caught_fish)
    }
    
    # Results if catch all fish
    if (n_caught == n_fish) {
      successful_runs <- successful_runs + 1L
      num_columns <- sum(columns_used)
      
      # Update
      total_draws_sum <- total_draws_sum + total_draws
      total_draws_sq_sum <- total_draws_sq_sum + total_draws^2
      columns_used_sum <- columns_used_sum + num_columns
      columns_used_sq_sum <- columns_used_sq_sum + num_columns^2
      
      min_draws <- min(min_draws, total_draws)
      max_draws <- max(max_draws, total_draws)
      min_cols <- min(min_cols, num_columns)
      max_cols <- max(max_cols, num_columns)
      
      # Best solution?
      composite_score <- (1 - column_minimization_weight) * total_draws + 
        column_minimization_weight * num_columns * scale_factor
      
      if (composite_score < best_score) {
        best_score <- composite_score
        best_solution <- list(
          simulation = sim,
          total_draws = total_draws,
          num_columns = num_columns,
          columns_used = time_columns[columns_used],
          composite_score = composite_score,
          caught_all_fish = TRUE
        )
      }
    }
  }
  
  #  final stats
  if (successful_runs > 0) {
    mean_draws <- total_draws_sum / successful_runs
    mean_cols <- columns_used_sum / successful_runs
    
    # sds
    var_draws <- (total_draws_sq_sum / successful_runs) - mean_draws^2
    var_cols <- (columns_used_sq_sum / successful_runs) - mean_cols^2
    
    summary_stats <- list(
      success_rate = successful_runs / N,
      mean_total_draws = mean_draws,
      sd_total_draws = sqrt(max(0, var_draws)),
      min_total_draws = min_draws,
      max_total_draws = max_draws,
      mean_num_columns = mean_cols,
      sd_num_columns = sqrt(max(0, var_cols)),
      min_num_columns = min_cols,
      max_num_columns = max_cols,
      successful_runs = successful_runs
    )
  } else {
    summary_stats <- list(
      success_rate = 0,
      successful_runs = 0
    )
  }
  
  return(list(
    best_solution = best_solution,
    summary_stats = summary_stats,
    parameters = list(
      N = N,
      exploration_weight = exploration_weight,
      column_minimization_weight = column_minimization_weight
    )
  ))
}


tune_parameters <- function(df, param_grid, N = 5000) {
  #' Quick parameter tuning function
  #' @param df Input dataframe
  #' @param param_grid Data frame with exploration_weight and column_minimization_weight columns
  #' @param N Number of simulations per parameter combination
  
  results <- vector("list", nrow(param_grid))
  
  for (i in seq_len(nrow(param_grid))) {
    cat("Testing parameters:", param_grid$exploration_weight[i], 
        param_grid$column_minimization_weight[i], "\n")
    
    result <- optimize_fish_catching(
      df, N = N,
      exploration_weight = param_grid$exploration_weight[i],
      column_minimization_weight = param_grid$column_minimization_weight[i]
    )
    
    results[[i]] <- c(
      param_grid[i, ],
      success_rate = result$summary_stats$success_rate,
      mean_draws = ifelse(is.null(result$summary_stats$mean_total_draws), NA, 
                          result$summary_stats$mean_total_draws),
      mean_cols = ifelse(is.null(result$summary_stats$mean_num_columns), NA,
                         result$summary_stats$mean_num_columns),
      best_score = ifelse(is.null(result$best_solution), NA, 
                          result$best_solution$composite_score)
    )
  }
  
  do.call(rbind, lapply(results, as.data.frame))
}

create_optimal_fishing_schedule <- function(df, optimal_columns, 
                                            sort_by = "probability", 
                                            min_probability = 0,
                                            wide_format = TRUE) {
  #' Creates a detailed fishing schedule table from optimal time periods
  #' 
  #' @param df Original dataframe with fish data
  #' @param optimal_columns Vector of optimal time column names (from optimization result)
  #' @param sort_by How to sort within each location: "probability", "name", or "none"
  #' @param min_probability Minimum probability threshold
  #' @param wide_format If TRUE, returns wide format with columns for each time period
  #' @return Data frame with fishing schedule organized by location and time
  
  library(data.table)
  library(tidyr)
  
  if (is.null(optimal_columns) || length(optimal_columns) == 0) {
    stop("No optimal columns provided. Run optimization first.")
  }
  
  dt <- as.data.table(df)
  
  # Check column names
  time_columns <- names(dt)[!names(dt) %in% c('name', 'location_collapsed')]
  missing_cols <- setdiff(optimal_columns, time_columns)
  if (length(missing_cols) > 0) {
    warning("Optimal columns not found in data: ", paste(missing_cols, collapse = ", "))
    optimal_columns <- intersect(optimal_columns, time_columns)
  }
  
  if (length(optimal_columns) == 0) {
    stop("No valid optimal columns found in dataset.")
  }

  schedule_data <- list()
  
  for (time_col in optimal_columns) {
    temp_dt <- dt[, .(
      name = name,
      location_collapsed = location_collapsed,
      time_period = time_col,
      probability = get(time_col)
    )]

    temp_dt <- temp_dt[probability >= min_probability]
    
    schedule_data[[time_col]] <- temp_dt
  }

  schedule_dt <- rbindlist(schedule_data)
  
  if (nrow(schedule_dt) == 0) {
    warning("No fish meet the minimum probability threshold.")
    return(data.frame())
  }
  
  schedule_dt[, `:=`(
    location_fish_count = .N,
    location_total_prob = sum(probability),
    location_avg_prob = mean(probability),
    location_max_prob = max(probability)
  ), by = .(location_collapsed, time_period)]
  
  schedule_dt[, prob_rank := rank(-probability, ties.method = "min"), 
              by = .(location_collapsed, time_period)]

  if (sort_by == "probability") {
    setorder(schedule_dt, location_collapsed, time_period, -probability, name)
  } else if (sort_by == "name") {
    setorder(schedule_dt, location_collapsed, time_period, name)
  } else {
    setorder(schedule_dt, location_collapsed, time_period)
  }
  
  schedule_df <- as.data.frame(schedule_dt)
  
  # Return wide format
  if (wide_format) {
    wide_df <- schedule_df %>%
      select(name, location_collapsed, time_period, probability) %>%
      pivot_wider(
        names_from = time_period,
        values_from = probability,
        values_fill = 0
      ) %>%
      arrange(location_collapsed, name)
    
    return(wide_df)
  }
  
  return(schedule_df)
}

summarize_fishing_schedule <- function(schedule_df, by_location = TRUE, by_time = TRUE) {
  #' Creates summary statistics from the fishing schedule
  #' 
  #' @param schedule_df Output from create_optimal_fishing_schedule()
  #' @param by_location Include location-level summaries
  #' @param by_time Include time-period summaries
  #' @return List of summary tables
  
  if (nrow(schedule_df) == 0) {
    return(list(message = "No data to summarize"))
  }
  
  summaries <- list()
  
  #  summary
  summaries$overall <- data.frame(
    total_fish_opportunities = nrow(schedule_df),
    unique_fish = length(unique(schedule_df$name)),
    unique_locations = length(unique(schedule_df$location_collapsed)),
    unique_time_periods = length(unique(schedule_df$time_period)),
    avg_probability = mean(schedule_df$probability),
    median_probability = median(schedule_df$probability),
    min_probability = min(schedule_df$probability),
    max_probability = max(schedule_df$probability)
  )
  
  if (by_location) {
    summaries$by_location <- schedule_df %>%
      group_by(location_collapsed) %>%
      summarise(
        unique_fish = n_distinct(name),
        unique_time_periods = n_distinct(time_period),
        total_opportunities = n(),
        avg_probability = mean(probability),
        median_probability = median(probability),
        max_probability = max(probability),
        high_prob_fish = sum(probability >= 0.5),  
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_probability))
  }
  
  # time summaries
  if (by_time) {
    summaries$by_time_period <- schedule_df %>%
      group_by(time_period) %>%
      summarise(
        unique_fish = n_distinct(name),
        unique_locations = n_distinct(location_collapsed),
        total_opportunities = n(),
        avg_probability = mean(probability),
        median_probability = median(probability),
        max_probability = max(probability),
        high_prob_fish = sum(probability >= 0.5),
        .groups = 'drop'
      ) %>%
      arrange(desc(avg_probability))
  }
  
  # top fish by probability in each location-time combo
  summaries$best_opportunities <- schedule_df %>%
    group_by(location_collapsed, time_period) %>%
    slice_max(probability, n = 3, with_ties = FALSE) %>%
    arrange(location_collapsed, time_period, desc(probability)) %>%
    select(location_collapsed, time_period, name, probability, prob_rank)
  
  return(summaries)
}

optimize_fish_catching_with_viz_data <- function(df, 
                                                 N = 10000,
                                                 exploration_weight = 0.3,
                                                 column_minimization_weight = 0.5) {
  
  dt <- as.data.table(df)
  
  # Pre-compute static elements
  time_columns <- names(dt)[!names(dt) %in% c('name', 'location_collapsed')]
  n_time_cols <- length(time_columns)
  fish_names <- dt$name
  n_fish <- length(fish_names)
  locations <- unique(dt$location_collapsed)
  n_locations <- length(locations)
  
  # Create lookup tables
  fish_to_idx <- setNames(seq_len(n_fish), fish_names)
  col_to_idx <- setNames(seq_len(n_time_cols), time_columns)
  
  # Probability matrices
  location_fish_idx <- vector("list", n_locations)
  prob_matrices <- vector("list", n_locations)
  nonzero_masks <- vector("list", n_locations)  
  names(location_fish_idx) <- locations
  names(prob_matrices) <- locations
  names(nonzero_masks) <- locations
  
  for (i in seq_along(locations)) {
    loc <- locations[i]
    loc_rows <- dt$location_collapsed == loc
    location_fish_idx[[i]] <- which(loc_rows)
    prob_matrix <- as.matrix(dt[loc_rows, ..time_columns])
    prob_matrices[[i]] <- prob_matrix
    nonzero_masks[[i]] <- prob_matrix > 0
  }
  
  # Track detailed simulation results for visualization
  detailed_results <- data.frame(
    simulation = integer(),
    total_draws = integer(),
    num_columns = integer(),
    success = logical(),
    exploration_choices = integer(),
    exploitation_choices = integer()
  )
  
  # Empty vectors
  best_score <- Inf
  best_solution <- NULL
  
  # Summary stats
  successful_runs <- 0
  total_draws_sum <- 0
  total_draws_sq_sum <- 0
  columns_used_sum <- 0
  columns_used_sq_sum <- 0
  min_draws <- Inf
  max_draws <- 0
  min_cols <- Inf
  max_cols <- 0
  
  # Simulation parameters
  exploration_threshold <- exploration_weight
  col_penalty <- column_minimization_weight * 0.1
  scale_factor <- 50
  max_attempts <- 1000
  
  # Main simulation
  for (sim in seq_len(N)) {
    if (sim %% 2000 == 0) {
      cat("Simulation", sim, "/", N, 
          "- Success rate:", round(successful_runs/sim * 100, 1), "%\n")
    }
    
    caught_fish <- logical(n_fish)
    columns_used <- logical(n_time_cols)
    total_draws <- 0L
    column_usage_count <- integer(n_time_cols)
    exploration_count <- 0L
    exploitation_count <- 0L
    
    attempt <- 0L
    n_caught <- 0L
    
    while (n_caught < n_fish && attempt < max_attempts) {
      attempt <- attempt + 1L
      
      # Column selection
      if (runif(1) < exploration_threshold || sum(columns_used) == 0) {
        exploration_count <- exploration_count + 1L
        if (runif(1) < 0.5) {
          chosen_col_idx <- sample.int(n_time_cols, 1)
        } else {
          if (sum(column_usage_count) > 0) {
            min_usage <- min(column_usage_count)
            candidate_cols <- which(column_usage_count == min_usage)
            chosen_col_idx <- sample(candidate_cols, 1)
          } else {
            chosen_col_idx <- sample.int(n_time_cols, 1)
          }
        }
      } else {
        exploitation_count <- exploitation_count + 1L
        # Exploitation
        column_scores <- numeric(n_time_cols)
        
        for (loc_idx in seq_along(locations)) {
          uncaught_in_loc <- location_fish_idx[[loc_idx]][!caught_fish[location_fish_idx[[loc_idx]]]]
          
          if (length(uncaught_in_loc) > 0) {
            loc_probs <- prob_matrices[[loc_idx]]
            uncaught_local_idx <- match(uncaught_in_loc, location_fish_idx[[loc_idx]])
            
            probs_uncaught <- loc_probs[uncaught_local_idx, , drop = FALSE]
            prob_catch_none <- apply(1 - probs_uncaught, 2, prod)
            prob_catch_at_least_one <- 1 - prob_catch_none
            
            column_scores <- column_scores + prob_catch_at_least_one * length(uncaught_in_loc)
          }
        }
        
        penalty_multiplier <- ifelse(columns_used, 1 + col_penalty, 1 - col_penalty)
        column_scores <- column_scores * penalty_multiplier
        column_scores <- pmax(column_scores, 0) 
        
        total_score <- sum(column_scores)
        if (total_score > 0 && !any(is.na(column_scores)) && !any(is.infinite(column_scores))) {
          chosen_col_idx <- sample.int(n_time_cols, 1, prob = column_scores)
        } else {
          chosen_col_idx <- sample.int(n_time_cols, 1)
        }
      }
      
      columns_used[chosen_col_idx] <- TRUE
      column_usage_count[chosen_col_idx] <- column_usage_count[chosen_col_idx] + 1L
      
      # Simulate fishing
      new_catches <- 0L
      for (loc_idx in seq_along(locations)) {
        fish_in_loc <- location_fish_idx[[loc_idx]]
        uncaught_in_loc <- fish_in_loc[!caught_fish[fish_in_loc]]
        
        if (length(uncaught_in_loc) > 0) {
          uncaught_local_idx <- match(uncaught_in_loc, fish_in_loc)
          
          if (any(nonzero_masks[[loc_idx]][uncaught_local_idx, chosen_col_idx])) {
            probs <- prob_matrices[[loc_idx]][uncaught_local_idx, chosen_col_idx]
            
            nonzero_fish <- probs > 0
            if (any(nonzero_fish)) {
              nonzero_probs <- probs[nonzero_fish]
              nonzero_indices <- uncaught_in_loc[nonzero_fish]
              
              catches <- runif(length(nonzero_probs)) < nonzero_probs
              caught_indices <- nonzero_indices[catches]
              
              if (length(caught_indices) > 0) {
                caught_fish[caught_indices] <- TRUE
                new_catches <- new_catches + length(caught_indices)
              }
            }
          }
        }
      }
      
      total_draws <- total_draws + max(1L, new_catches)
      n_caught <- sum(caught_fish)
    }
    
    # Store detailed results for this simulation
    success <- (n_caught == n_fish)
    detailed_results <- rbind(detailed_results, data.frame(
      simulation = sim,
      total_draws = total_draws,
      num_columns = sum(columns_used),
      success = success,
      exploration_choices = exploration_count,
      exploitation_choices = exploitation_count
    ))
    
    # Results if catch all fish
    if (n_caught == n_fish) {
      successful_runs <- successful_runs + 1L
      num_columns <- sum(columns_used)
      
      total_draws_sum <- total_draws_sum + total_draws
      total_draws_sq_sum <- total_draws_sq_sum + total_draws^2
      columns_used_sum <- columns_used_sum + num_columns
      columns_used_sq_sum <- columns_used_sq_sum + num_columns^2
      
      min_draws <- min(min_draws, total_draws)
      max_draws <- max(max_draws, total_draws)
      min_cols <- min(min_cols, num_columns)
      max_cols <- max(max_cols, num_columns)
      
      composite_score <- (1 - column_minimization_weight) * total_draws + 
        column_minimization_weight * num_columns * scale_factor
      
      if (composite_score < best_score) {
        best_score <- composite_score
        best_solution <- list(
          simulation = sim,
          total_draws = total_draws,
          num_columns = num_columns,
          columns_used = time_columns[columns_used],
          composite_score = composite_score,
          caught_all_fish = TRUE
        )
      }
    }
  }
  
  # final stats
  if (successful_runs > 0) {
    mean_draws <- total_draws_sum / successful_runs
    mean_cols <- columns_used_sum / successful_runs
    
    var_draws <- (total_draws_sq_sum / successful_runs) - mean_draws^2
    var_cols <- (columns_used_sq_sum / successful_runs) - mean_cols^2
    
    summary_stats <- list(
      success_rate = successful_runs / N,
      mean_total_draws = mean_draws,
      sd_total_draws = sqrt(max(0, var_draws)),
      min_total_draws = min_draws,
      max_total_draws = max_draws,
      mean_num_columns = mean_cols,
      sd_num_columns = sqrt(max(0, var_cols)),
      min_num_columns = min_cols,
      max_num_columns = max_cols,
      successful_runs = successful_runs
    )
  } else {
    summary_stats <- list(
      success_rate = 0,
      successful_runs = 0
    )
  }
  
  return(list(
    best_solution = best_solution,
    summary_stats = summary_stats,
    detailed_results = detailed_results,
    parameters = list(
      N = N,
      exploration_weight = exploration_weight,
      column_minimization_weight = column_minimization_weight
    )
  ))
}

create_monte_carlo_visualizations <- function(optimization_result) {
  
  detailed_data <- optimization_result$detailed_results
  successful_data <- detailed_data[detailed_data$success == TRUE, ]
  
  if (nrow(successful_data) == 0) {
    return(list(
      draws_histogram = ggplot() + labs(title = "No successful simulations to visualize"),
      exploration_effect = ggplot() + labs(title = "No successful simulations to visualize"),
      columns_histogram = ggplot() + labs(title = "No successful simulations to visualize")
    ))
  }
  
  # Visualization 1: Histogram of draws needed
  plot1 <- ggplot(successful_data, aes(x = total_draws)) +
    geom_histogram(bins = 30, fill = "#dda15e", color = "#bc6c25", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(total_draws)), 
               color = "#283618", linetype = "dashed", size = 1.2) +
    annotate("text", 
             x = mean(successful_data$total_draws) + 5, 
             y = Inf, 
             label = paste("Mean:", round(mean(successful_data$total_draws), 1)), 
             vjust = 2, color = "#283618", fontface = "bold") +
    labs(
      title = "Distribution of Total Draws Needed",
      subtitle = paste("Based on", nrow(successful_data), "successful simulations"),
      x = "Total Fishing Attempts",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "#283618", face = "bold", size = 14),
      plot.subtitle = element_text(color = "#606c38", size = 11),
      axis.title = element_text(color = "#283618", face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fefae0", color = NA)
    )
  
  # Visualization 2: Exploration vs exploitation choices
  exploration_ratio <- successful_data$exploration_choices / 
    (successful_data$exploration_choices + successful_data$exploitation_choices)
  
  plot2 <- ggplot(successful_data, aes(x = exploration_ratio, y = total_draws)) +
    geom_point(color = "#bc6c25", alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = TRUE, color = "#283618", fill = "#dda15e", alpha = 0.3) +
    labs(
      title = "Exploration Ratio vs Efficiency",
      subtitle = "Relationship between exploration choices and draws needed",
      x = "Proportion of Exploration Choices",
      y = "Total Draws Needed"
    ) +
    scale_x_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "#283618", face = "bold", size = 14),
      plot.subtitle = element_text(color = "#606c38", size = 11),
      axis.title = element_text(color = "#283618", face = "bold"),
      panel.background = element_rect(fill = "#fefae0", color = NA)
    )
  
  # Visualization 3: Histogram of unique columns used
  plot3 <- ggplot(successful_data, aes(x = num_columns)) +
    geom_histogram(bins = 15, fill = "#606c38", color = "#283618", alpha = 0.8) +
    geom_vline(aes(xintercept = mean(num_columns)), 
               color = "#bc6c25", linetype = "dashed", size = 1.2) +
    annotate("text", 
             x = mean(successful_data$num_columns) + 0.5, 
             y = Inf, 
             label = paste("Mean:", round(mean(successful_data$num_columns), 1)), 
             vjust = 2, color = "#bc6c25", fontface = "bold") +
    labs(
      title = "Distribution of Time Periods Used",
      subtitle = "Number of unique time slots needed per successful simulation",
      x = "Number of Time Periods",
      y = "Frequency"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_minimal() +
    theme(
      plot.title = element_text(color = "#283618", face = "bold", size = 14),
      plot.subtitle = element_text(color = "#606c38", size = 11),
      axis.title = element_text(color = "#283618", face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fefae0", color = NA)
    )
  
  return(list(
    draws_histogram = plot1,
    exploration_effect = plot2,
    columns_histogram = plot3
  ))
}

generate_exploration_comparison <- function(df, exploration_weights = seq(0.1, 0.9, 0.1), N = 1000) {
  results <- data.frame()
  
  for (weight in exploration_weights) {
    cat("Testing exploration weight:", weight, "\n")
    result <- optimize_fish_catching_with_viz_data(
      df, 
      N = N,
      exploration_weight = weight,
      column_minimization_weight = 0.5  # Keep this constant
    )
    
    results <- rbind(results, data.frame(
      exploration_weight = weight,
      success_rate = result$summary_stats$success_rate,
      mean_draws = ifelse(result$summary_stats$success_rate > 0, 
                          mean(result$detailed_results$total_draws[result$detailed_results$success]), 
                          NA),
      mean_columns = ifelse(result$summary_stats$success_rate > 0,
                            mean(result$detailed_results$num_columns[result$detailed_results$success]),
                            NA)
    ))
  }
  
  return(results)
}

