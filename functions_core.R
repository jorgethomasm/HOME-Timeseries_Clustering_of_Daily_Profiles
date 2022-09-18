# Functions for main.R ----------------------


#' Is Heating Degree Day? (is_HDD)
#' 
#' @param df_data_hourly A data frame with datetime index, power and ambient temperature values.
#' @param hdd_threshold a number (threshold) in degree Celsius, e.g.: 16
#' @return the hourly input df.data houry with the boolean column is_HDD
#' @examples
#' is_HDD(df_hourly, 16)

is_HDD <- function (df_data_hourly, 
                    
                    hdd_threshold){
  
  
  df_temp_hourly <- data.frame(datetime_idx = df_data_hourly[, 1],
                               
                                      T_obs = df_data_hourly$T_obs)
  
  T_daily_agg <- data.frame(
    
    aggregate(df_temp_hourly, by = list(as.Date(cut(df_temp_hourly$datetime_idx, "days"))), FUN = mean, na.rm = TRUE)[, c(1,3)]
    
  )
  
  colnames(T_daily_agg) <- c("index", "T_obs_day")
  
  T_daily_agg$is_HDD <- TRUE # Create and set all TRUE (pupulate) 
  
  T_daily_agg$is_HDD[which(T_daily_agg$T_obs_day > hdd_threshold)] <- FALSE
  
  idx_no_HDD <- T_daily_agg$index[which(T_daily_agg$is_HDD == FALSE)]
  
  df_data_hourly$is_HDD <- TRUE # Add column and set all TRUE
  
  df_data_hourly$is_HDD[which(as.Date(df_data_hourly[, 1]) %in% idx_no_HDD)] <- FALSE
  
  return(df_data_hourly$is_HDD)
  
}



#' Add statistics to training data (df_train) for analysis (prepare_df)
#' #' ** THIS FUNCION REQUIRES "is_HDD" function **
#' @param df_train A data frame / time table with datetime index, power and ambient temperature values.
#' @param hdd_threshold a number (threshold) in degree Celsius, e.g.: 16
#' @return df_train with added columns of statistics
#' @examples
#' prepare_df(df_train, 16)

prepare_df <- function(x = df_train){
  
  
  #  Power (Load):
  
  P_daily_avg <- aggregate(df_train$P_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = mean, na.rm = TRUE)[,2]
  
  P_daily_med <- aggregate(df_train$P_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = median, na.rm = TRUE)[,2]
  
  P_daily_sde <- aggregate(df_train$P_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = sd, na.rm = TRUE)[,2]
  
  P_daily_max <- aggregate(df_train$P_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = max, na.rm = TRUE)[,2]
  
  
  # Temperature:
  
  T_daily_avg <- aggregate(df_train$T_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = mean, na.rm = TRUE)[,2]
  
  T_daily_med <- aggregate(df_train$T_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = median, na.rm = TRUE)[,2]
  
  T_daily_sde <- aggregate(df_train$T_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = sd, na.rm = TRUE)[,2]
  
  T_daily_max <- aggregate(df_train$T_obs, list(as.Date(cut(df_train[, 1], "days"))), FUN = max, na.rm = TRUE)[,2]
  
  
  # Add columns (variables) to traing data (df_train)
  
  df_train$t_hh      <- as.integer(format(df_train[, 1], "%H")) # t_hh: hour of day
  
  df_train$P_pu      <- df_train$P_obs /  max(df_train$P_obs, na.rm = TRUE) # per unit [p.u]
  df_train$P_day_avg <- rep(P_daily_avg, each = 24)
  df_train$P_day_med <- rep(P_daily_med, each = 24)
  df_train$P_day_sde <- rep(P_daily_sde, each = 24)
  df_train$P_day_max <- rep(P_daily_max, each = 24)
  df_train$P_scaled  <- df_train$P_obs / df_train$P_day_max # Normalised daily power (focus on shape)
  
  df_train$T_pu      <- df_train$T_obs /  max(df_train$T_obs, na.rm = T) # per unit [p.u]
  df_train$T_day_avg <- rep(T_daily_avg, each = 24)
  df_train$T_day_med <- rep(T_daily_med, each = 24)
  df_train$T_day_sde <- rep(T_daily_sde, each = 24)
  df_train$T_day_max <- rep(T_daily_max, each = 24)
  df_train$T_scaled  <- df_train$T_obs / df_train$T_day_max # Normalised daily temperature
  
  # Is Heating Degree Day?
  # df_train$is_HDD    <- is_HDD(df_train, hdd_threshold)
  
  # Date Labels
  df_train$date      <- format(as.Date(df_train[, 1]))
  df_train$day_name  <- format(as.Date(df_train[, 1]), "%a")
  
  return(df_train)
}



#' This function reorders (relabel) clusters numbers in function of 
#' increasing Pavg per cluster. Useful for visualisation
#' @param df_HAC_cut cutree as data frame.
#' @param P_daily_avg aggregated mean daily power 
#' @return reassingned HAC_cut
#' @examples
#' reassign_clusters(df, df)
reassign_clusters <- function(df_HAC_cut, P_daily_avg){
  
  df_HAC_cut$P_day_avg  <- P_daily_avg$`df_train$P_obs`
  
  colnames(df_HAC_cut) <- c("cluster", "P_avg_day") 
  
  df_HAC_cut$cluster_copy <- df_HAC_cut$cluster
  
  df_P_avg_perCluster <- setNames(aggregate(df_HAC_cut$P_avg_day ~ cluster, df_HAC_cut, mean), c("cluster", "P_avg_day"))
  
  # Reorder:
  
  df_P_avg_perCluster <- df_P_avg_perCluster[order(df_P_avg_perCluster$P_avg_day), ]
  
  df_P_avg_perCluster$cluster_new <- seq(1, K)
  
  df_HAC_cut$cluster <- NA
  
  for (i in 1:K) {
    
    idxs <- which(df_HAC_cut$cluster_copy == df_P_avg_perCluster$cluster[i])
    
    df_HAC_cut$cluster[idxs] <- df_P_avg_perCluster$cluster_new[i]
    
  }
  
  HAC_cut <- as.vector(df_HAC_cut$cluster)
  
  names(HAC_cut) <- rownames(df_HAC_cut)
  
  return(HAC_cut)
}



#' Plot clusters with hourly density functions (df_train) for analysis (plot_cluster)#' 
#' @param df_train A data frame / time table with datetime index, power and ambient temperature values.
#' @param df_clusters_stats a data frame with statistics for power and Temp. per cluster, i.e. avg, median, sde, p
#' @param cluster_number k = 1,2,3....
#' @param colour_palette a character vector of hex colours
#' @param is_normalised a boolean
#' @return plot objects
#' @examples
#' plot_cluster(df_train, 1, my_colour_palette, FALSE)

plot_cluster <- function(df_train,
                         
                         df_clusters_stats,
                         
                         cluster_number,
                         
                         colour_palette, 
                         
                         is_normalised){
  
  max_Y <- max(df_train$P_obs, na.rm = TRUE)
  
  if (max_Y < 1000 && max_Y >= 10 ) {
    
    max_Y_axis <- ceiling(max(df_train$P_obs, na.rm = TRUE) / 100) * 100
    
  }else if(max_Y < 10){
    
    # max_Y_axis <- ceiling(max(df_train$P_obs, na.rm = TRUE) / 10) * 10
    max_Y_axis <- ceiling(max(df_train$P_obs, na.rm = TRUE))
    
  }
  
  div_Y <- as.numeric(substr(as.character(max_Y_axis),1,1)) # for plot minor scale
  
  K <- max(df_train$cluster, na.rm = TRUE)
  
  # Subsetting:
  
  df_cluster = subset(df_train, df_train$cluster == cluster_number)
  
  df_cluster_stats <- subset(df_clusters_stats, df_clusters_stats$cluster == cluster_number)
  
  
  # PLOT time-series:
  
  plot_ts <- ggplot(df_cluster, aes(x = t_hh, y = P_obs, group = dend_labels)) + 
    
    geom_line(aes(colour = T_day_avg)) + 
      
    scale_colour_gradient2(low = colour_palette[length(colour_palette)], high = colour_palette[1], midpoint = median(df_cluster$T_day_med)) +
      
    xlab("UTC") 
    
    if(is_normalised){
      
      plot_ts <- plot_ts + ylab("") +
        
        scale_y_continuous(breaks = seq(0, 1, by = 1), minor_breaks =  waiver(), limits = c(0, 1))
      
    }else{
      
      plot_ts <- plot_ts + ylab(paste0("y\n", my_y_unit)) +
        
        scale_y_continuous(breaks = seq(0, max_Y_axis, by = max_Y_axis/div_Y), minor_breaks =  waiver(), limits = c(0, max_Y_axis))
      
    }
  
    
  
  # Stats per time step:
  
  df_timestep_stats <- data.frame(
    
    avg = aggregate(df_cluster[, 2] ~ t_hh, df_cluster, mean, na.rm = TRUE)[,2],
    
    sde = aggregate(df_cluster[, 2] ~ t_hh, df_cluster, sd, na.rm = TRUE)[,2],
    
    med = aggregate(df_cluster[, 2] ~ t_hh, df_cluster, median, na.rm = TRUE)[,2])
  
  df_timestep_stats$upper <- df_timestep_stats$avg + df_timestep_stats$sde
  
  df_timestep_stats$lower <- df_timestep_stats$avg - df_timestep_stats$sde
  
  df_timestep_stats$t_hh <- seq(0, nrow(df_timestep_stats) - 1, 1)
  
  
  # Violin Plot 
  cluster_plot <- ggplot(data = df_cluster, aes(y = P_obs)) +
    
    # geom_half_violin(aes(x = t_hh, group = t_hh), fill = colour_palette[cluster_number], scale = "width", side = "r", alpha = 0.2, colour = "grey") +
    
    geom_point(aes(x = t_hh), size = 0.2, colour = colour_palette[cluster_number]) +
    
    geom_line(data = df_timestep_stats, aes(x = t_hh, y = med), size = 0.5, colour = "black") +
    
    scale_x_continuous(breaks =  seq(0, 23, by = 2), minor_breaks = waiver(), limits = c(0, 23)) +
    
    xlab("UTC")
  
  ifelse(cluster_number == K, 
         cluster_plot <- cluster_plot + ylab(paste0("y\n", my_y_unit)), 
         cluster_plot <- cluster_plot + ylab("") + xlab("") + theme(axis.text.y = element_blank())
         )
  
  if(is_normalised){
    
    cluster_plot <- cluster_plot + ylab("") +
      
      scale_y_continuous(breaks = seq(0, 1, by = 0.1), minor_breaks = waiver(), limits = c(0, 1)) +
      
      annotate("text", x = 11.5, y = 0.98, label = paste("italic(p)(k) ==", df_cluster_stats$probability), size = 3, colour = "black", parse = TRUE) +
      
      annotate("text", x = 11.5, y = 0.92, label = paste("bar(T) ==", round(df_cluster_stats$T_avg, 2)), size = 2.5, colour = "black", parse = TRUE) +
      
      annotate("text", x = 11.5, y = 0.86, label = paste("bar(y) ==", round(df_cluster_stats$P_avg, 2)), size = 2.5, colour = "black", parse = TRUE)
      
    
  }else{
      
    cluster_plot <- cluster_plot + 
      
      scale_y_continuous(breaks = seq(0, max_Y_axis, by = max_Y_axis/div_Y), minor_breaks = waiver(), limits = c(0, max_Y_axis)) +
      
      annotate("text", x = 11.5, y = 0.98*max_Y_axis, label = paste("italic(p)(k) ==", df_cluster_stats$probability), size = 2.5, colour = "black", parse = TRUE) +
      
      annotate("text", x = 11.5, y = 0.92*max_Y_axis, label = paste("bar(T) ==", round(df_cluster_stats$T_avg, 2)), size = 2.5, colour = "black", parse = TRUE) +
      
      annotate("text", x = 11.5, y = 0.86*max_Y_axis, label = paste("bar(y) ==", round(df_cluster_stats$P_avg, 2)), size = 2.5, colour = "black", parse = TRUE)
    
  }
  
  
  # List of objects to return:
  
  list_of_results <- list(df = df_timestep_stats, plot_ts = plot_ts, cluster_plot = cluster_plot)
   
  return(list_of_results)
  
}
