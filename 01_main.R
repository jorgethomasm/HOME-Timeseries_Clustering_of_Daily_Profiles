#' The following script read time-series data and generates the plots
#' 
#' 
#' J. Thomas
#' jorgethomasm@ieee.org

source("00_settings.R") # Load settings and inputs

# 0. Load / Read Data ----------------------------------------------

# IND:
# load("./data_examples/ind_iosb_id61_20171012_20190205_UTC.Rdata")

# URB:
load("./data_examples/urb_20151012_20161011_UTC.Rdata")

# SUB:
# load("./data_examples/sub_20151012_20161011_UTC.Rdata")

# MESSKIRCH:
# load("./data_examples/mess_20190204_20200301_UTC.Rdata")

# LOERRACH Pelec
# load("./data_examples/Loerrach_Pelec_WP_20191217_20200220_UTC.Rdata")

# LOERRACH Pheat
# load("./data_examples/Loerrach_Pheat_WP_20191217_20200220_UTC.Rdata")

# 1. Prepare Data Frame --------------------------------------------


df_train <- prepare_df(df_train)

# for shape emphasis:
if(is_normalised) df_train$P_obs <- df_train$P_scaled


# 3. Data Frame to Matrix ------------------------------------------

#' Explicit dependence on time scale f(day, hours)
#' 
#' Transposed Matrix --> hclust combines between ROWS

mat_P <- t(matrix(df_train$P_obs, nrow = 24, byrow = FALSE)) # 288 for 5 min; 48 for 30 min; 24 for 1 hour

n_days <- nrow(mat_P)

# For rownames:

T_daily_avg <- aggregate(df_train$T_obs, list(as.Date(cut(df_train$datetime_idx, "days"))), mean)[, 2]

P_daily_avg <- aggregate(df_train$P_obs ~ date, df_train, mean) # For HAC_cut match

# Naming (Select desired FORMAT):

row_labels <- paste(strftime(unique(as.Date(df_train$datetime_idx)), format = "%a %Y-%m-%d"), format(round(T_daily_avg, 1), nsmall = 1))

rownames(mat_P) <- row_labels

colnames(mat_P) <- format(df_train$datetime_idx[1:24], "%R")

# mat_P <- mat_P[order(rowMeans(mat_P)), ] # reorder rows min Pavg to max Pavg




# 4. CLUSTERING -----------------------------------------------------

dd <- dist(mat_P, method = "euclidean") # distances between rows of a matrix

# print(dd, digits = 4) # print distance matrix

HAC <- hclust(dd, method = "complete")  #  linkage

my_dendro <- rev(as.dendrogram(HAC)) # From colder (less leaves) to hotter (more leaves)



# 4.1 Labeling ------------------------------------------------------

# Returns a named vector of integers

df_HAC_cut <- as.data.frame(cutree(HAC, K)) # here, order_clusters_as_data = TRUE by default

HAC_cut <- reassign_clusters(df_HAC_cut, P_daily_avg) # Reorder Clusters -

# Assigning Cluster:

df_train$dend_labels <- rep(rownames(mat_P), each = 24)

df_train$cluster <- NA 

for (i in seq(1,K)) {
  
  idxs_clust <- charmatch(df_train$dend_labels, names(HAC_cut[which(HAC_cut == i)]))
  
  df_train$cluster[which(!is.na(idxs_clust))] <- i # fill it up
  
}



# 4.2 Statistics 


# 4.2.1. Per Cluster -----------------------------------------

# This table can be printed!

df_stats_per_cluster <- data.frame(
  
  cluster = seq(1, K, 1),
  
  P_avg   = aggregate(df_train$P_obs ~ cluster, df_train, mean)[, 2],
  P_sde   = aggregate(df_train$P_obs ~ cluster, df_train, sd)[, 2],
  P_med   = aggregate(df_train$P_obs ~ cluster, df_train, median)[, 2],
  P_max   = aggregate(df_train$P_obs ~ cluster, df_train, max)[, 2],
  
  T_avg   = aggregate(df_train$T_obs ~ cluster, df_train, mean)[, 2],
  T_sde   = aggregate(df_train$T_obs ~ cluster, df_train, sd)[, 2],
  T_med   = aggregate(df_train$T_obs ~ cluster, df_train, median)[, 2],
  
  total_days  = sapply(seq(1,K), function(k){ length(which(HAC_cut == k)) }))

df_stats_per_cluster$E_day <- df_stats_per_cluster$P_avg * 24

df_stats_per_cluster$probability <- round(df_stats_per_cluster$total_days / sum(df_stats_per_cluster$total_days), digits = 3) # probability

df_stats_per_cluster$palette <- my_colour_palette



# ---- 4.2.1. Temp Daily ----

df_stats_temp_daily <- data.frame(
  
  date = unique(as.Date(df_train$datetime_idx)),
  
  T_daily_avg = T_daily_avg,
  T_daily_sde = aggregate(df_train$T_obs, list(as.Date(cut(df_train$datetime_idx, "days"))), sd)[, 2],
  T_daily_med = aggregate(df_train$T_obs, list(as.Date(cut(df_train$datetime_idx, "days"))), median)[, 2],
  
  cluster = as.factor(df_train$cluster[match(unique(df_train$date), df_train$date)])
)



# 5. VIS & PLOTS -------------------------------


# 5.1. Dendrogram ---------------------------------

# 5.1.1 Set plot's properties:

# Set size of labels and hang leaves depending on # days

my_cex <- 0.15

my_hang_leaves <- ifelse(n_days > 100, 0.01, -1)

my_dendro <- dendextend::set(my_dendro, "hang_leaves", my_hang_leaves)

list_labels_per_cluster <- lapply(1:K, function(k){ unique(df_train$dend_labels[which(df_train$cluster == k)])} )

list_colours_dendro <- vector(mode = "list", length = length(labels(my_dendro)))

for (k in 1:K) {
  
  list_colours_dendro[which(labels(my_dendro) %in% list_labels_per_cluster[[k]])] <- df_stats_per_cluster$palette[k]
  
}

my_dendro <- assign_values_to_leaves_edgePar(dend = my_dendro, value = unlist(list_colours_dendro), edgePar = "col")

plot_of_dendrogram <- factoextra::fviz_dend(my_dendro, 
                                            cex = my_cex,
                                            k = K, 
                                            lwd = 0.5,  
                                            #rect = TRUE, 
                                            #rect_fill = TRUE, 
                                            show_labels = TRUE, 
                                            k_colors = unique(unlist(list_colours_dendro)),
                                            color_labels_by_k = FALSE,
                                            main = "",
                                            sub = "",
                                            ylab = "",
                                            xlab = "[day yyyy-mm-dd T]",
                                            
                                            ggtheme = theme_classic() + 
                                              
                                              theme(axis.text.y = element_blank(),
                                                    axis.title.y = element_blank(),
                                                    axis.ticks.y = element_blank(),
                                                    axis.title.x = element_text(angle = 0, size = 8, vjust = 0.5, face = "bold")
                                                    )
                                                                              
                                            )

Sys.sleep(1.5)


# list_subdendro_plots <- lapply(dendextend::get_subdendrograms(my_dendro, K), ggplot)

# heights_per_k.dendrogram(my_dendro)



# 5.2. Shapes ---------------------------------

# debug(plot_cluster)
# 
# plot_cluster(df_train, df_stats_per_cluster, 1, my_colour_palette, is_normalised)

list_of_results <- lapply(seq(1,K), function(k) plot_cluster(df_train = df_train,
                                                   
                                                              df_clusters_stats = df_stats_per_cluster, 
                                                   
                                                              cluster_number = k, 
                                                   
                                                              colour_palette = my_colour_palette,
                                                   
                                                              is_normalised))

Sys.sleep(1)

# 5.2.1. Extract Results ---------------------------------

list_shape_plots <- lapply(seq(1,K), function(k) list_of_results[[k]]$cluster_plot)

list_tss_plots <- lapply(seq(1,K), function(k) list_of_results[[k]]$plot_ts)

list_hh_results_avg <- lapply(seq(1,K), function(k) list_of_results[[k]]$df$avg) # hh: hourly

list_hh_results_med <- lapply(seq(1,K), function(k) list_of_results[[k]]$df$med) # hh: hourly

# Unlist to df

# labels

colnames_avg  <- "k_1_Avg"
colnames_med  <- "k_1_Med"

for (k in 2:K) {
  
  colnames_avg <- append(colnames_avg, paste0("k_", k, "_Avg"))
  colnames_med <- append(colnames_med, paste0("k_", k, "_Med"))
  
}

df_results <- cbind(
  
  hour = seq(0, length(list_hh_results_med[[1]]) - 1, by = 1), 
  
  as.data.frame(list_hh_results_avg, col.names = colnames_avg),
                    
  as.data.frame(list_hh_results_med, col.names = colnames_med)
)



# 5.3. Temperature PDFs ----------------------------------

line_types <- c("Mean" = 1, "Median" = 2)

plots_of_pdf_temp <- ggplot(data = df_stats_temp_daily, aes(x = T_daily_avg, group = cluster)) + 
  
  geom_density(aes(fill = cluster, colour = cluster), alpha = 0.5) +
  
  scale_fill_manual(values = my_colour_palette, aesthetics = "fill") +
  
  guides(fill = FALSE, colour = FALSE) +
  
  geom_vline(data = df_stats_per_cluster, aes(xintercept = T_avg, colour = as.factor(cluster), linetype = "Mean")) +
  
  geom_vline(data = df_stats_per_cluster, aes(xintercept = T_med, colour = as.factor(cluster), linetype = "Median")) +
  
  scale_linetype_manual(name = "", values = line_types) + 

  scale_colour_manual(values = my_colour_palette, aesthetics = "color") +
  
  scale_x_continuous(breaks = seq(-15, hdd_threshold, by = 1), minor_breaks =  waiver()) +
  
  labs(x = expression(paste(bar(T)[day], " [°C]")), y = "PDF") + 
  
  theme(legend.position = c(0.88, 0.86))  




# 5.4.  Plot Patch ----------------------------------------------------


if (K == 2){
  
  plots_of_shapes <- list_shape_plots[[2]] + list_shape_plots[[1]] + plot_layout(ncol = K)
  
}else if(K == 3){
  
  plots_of_shapes <- list_shape_plots[[3]] + list_shape_plots[[2]] + list_shape_plots[[1]] + plot_layout(ncol = K)
  
}else if(K == 4){
  
  plots_of_shapes <- list_shape_plots[[4]] + list_shape_plots[[3]] + list_shape_plots[[2]] + list_shape_plots[[1]] + plot_layout(ncol = K)
  
}else if(K == 5){
  
  plots_of_shapes <- list_shape_plots[[5]] + list_shape_plots[[4]] + list_shape_plots[[3]] + list_shape_plots[[2]] + list_shape_plots[[1]] + 
    plot_layout(ncol = K)
  
}else if(K == 6){
  
  plots_of_shapes <- list_shape_plots[[6]] + list_shape_plots[[5]] + list_shape_plots[[4]] + list_shape_plots[[3]] + list_shape_plots[[2]] + 
    list_shape_plots[[1]] + plot_layout(ncol = K)
  
}else if(K == 7){
  
  plots_of_shapes <- list_shape_plots[[7]] + list_shape_plots[[6]] + list_shape_plots[[5]] + list_shape_plots[[4]] + list_shape_plots[[3]] + 
    list_shape_plots[[2]] + list_shape_plots[[1]] + plot_layout(ncol = K)
  
}


plot_of_everything <- plot_of_dendrogram /  plots_of_pdf_temp / plots_of_shapes



plot_of_everything <- plot_of_everything + plot_annotation(
  
  title = '',
  subtitle = '',
  caption = '')

plot(plot_of_everything)

Sys.sleep(3)


# 6. Save Results -----------------------------------------------------

# 6.1. Plots -----

# Square IEEE png
# ggsave(filename = "./plots/myIEEEplot.png", plot = plot_of_everything, width = 4.72, height = 4.72, units = "in", dpi = 320)

# A4 Landscape:
ggsave(filename = "./plots_and_results/plot_A4.pdf", plot = plot_of_everything,
       width  = df_papers_width_mm$A4heigth,
       height = df_papers_width_mm$A4width,
       units = "mm")

Sys.sleep(3)


# A4 Landscape .SVG:
ggsave(filename = "./plots_and_results/plot_A4.svg", plot = plot_of_everything,
       width  = df_papers_width_mm$A4heigth,
       height = df_papers_width_mm$A4width,
       units = "mm")

Sys.sleep(3)


# A4 png
ggsave(filename = "./plots_and_results/plot_A4.png", plot = plot_of_everything, width = 297, height = 210, units = "mm", dpi = 320)

# 6.2. Csv ----

write.csv(df_results, file = "./plots_and_results/results_hourly_k.csv", row.names = FALSE, quote = FALSE)


#' Conclusion:
#' 
#' The clustering parameters (linkage and distance type) were selected 
#' according to the most easy distribution obtained for the ambient temperature.
#' Probabilistic Assessment and Visualization of Heat Pumps Operation Modes
