library(tidyverse)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(cowplot)

# script for the comparison of simulated vs measured PAR values

# Read the wide data to calculate correlations
df_wide_ex <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide_explicit.csv") %>% 
  na.omit()
df_wide_vox02 <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide_vox02.csv") %>% 
  na.omit()
df_wide_vox01 <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide_vox1.csv") %>% 
  na.omit()

##################################################################################################

# join all datasets
dfwe <- df_wide_ex %>% 
  rename(simulated_ex = simulated) %>% 
  select(-time)

dfwv02 <- df_wide_vox02 %>% 
  select(-measured) %>% 
  rename(simulated_vox02 = simulated) %>% 
  select(-time)

dfwv01 <- df_wide_vox01 %>% 
  select(-measured) %>% 
  rename(simulated_vox01 = simulated) %>% 
  select(-time)

df_long <- left_join(dfwe, dfwv02, by = c("logger", "sensor", "time_local")) %>% 
  left_join(., dfwv01, by = c("logger", "sensor", "time_local")) %>% 
  pivot_longer(cols = -c("logger", "sensor", "time_local"), names_to = "type", values_to = "umolm2s") %>% 
  na.omit()

# filter which logger/ sensors to plot
df_filtered <- df_long %>%
  spread(key = type, value = umolm2s) %>%
  filter(!is.na(measured) & !is.na(simulated_ex)) %>%
  gather(key = type, value = umolm2s, measured, simulated_ex, simulated_vox02, simulated_vox01) %>% 
  filter((sensor %in% c(11, 7, 3, 6) & logger %in% c(2, 3, 4, 5, 6)) | (logger == 1 & sensor %in% c(9, 10, 11, 12)))

# Function to create a single plot for each logger
create_logger_plot <- function(df, logger_num) {
  logger_data <- df %>% filter(logger == logger_num)
  
  if (nrow(logger_data) == 0) {
    message(paste("Logger", logger_num, "has no data"))
    return(NULL)
  }
  
  correlation_ex <- unique(logger_data$pearson_r_ex)
  correlation_vox02 <- unique(logger_data$pearson_r_vox02)
  correlation_vox01 <- unique(logger_data$pearson_r_vox01)
  
  plot <- ggplot(logger_data, aes(time_local, umolm2s, color = type)) +
    geom_line() +
    facet_wrap(~sensor) +
    ggtitle(paste("Logger", logger_num)) +
    theme(axis.text = element_text(size = 7),
          title = element_text(size=9)) +
    labs(
      x = NULL, 
      y = expression(paste("PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")"))
    ) +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "grey", size = 0.1),
          panel.grid.minor = element_line(color = "lightgrey", size = 0.1),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 8),
          legend.position = "top",
          legend.title = element_blank(),
          title = element_text(size = 8),
          strip.text = element_text(size = 7, margin = margin(1.5, 1.5, 1.5, 1.5))) +  # Make facet strip box smaller
    ylim(c(0, 500)) +
    scale_y_continuous(breaks = seq(0, 1600, by = 600)) +
    scale_x_datetime(date_breaks = "1 hours", date_labels = "%H") +
    scale_color_brewer(palette = "Dark2", labels = c("Measured", "Expl. Geo.", "Vox. 1m", "Vox. 0.2m")) +
    ylab("")
  
  return(plot)
}

# Create plots for all loggers
logger_plots <- lapply(1:max(df_filtered$logger, na.rm = TRUE), function(logger_num) create_logger_plot(df_filtered, logger_num))

# Remove NULL plots
logger_plots <- logger_plots[!sapply(logger_plots, is.null)]

# Extract the legend from one of the plots
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(logger_plots[[1]])

# Remove legends from all plots
logger_plots <- lapply(logger_plots, function(p) p + theme(legend.position = "none"))

# Arrange plots
log_plot_arrange <- do.call(gridExtra::grid.arrange, c(logger_plots, ncol = 2))


###############################################################################

# Function to calculate correlations and plot
calculate_and_plot <- function(df, dataset_name, y_label = FALSE, x_label = FALSE) {
  # Calculate Pearson correlation coefficient for each sensor
  correlations_by_sensor <- df %>%
    group_by(logger, sensor) %>%
    summarise(pearson_r = cor(measured, simulated, use = "complete.obs"),
              .groups = 'drop')
  
  # Calculate Pearson correlation coefficient for each logger
  correlations_by_logger <- df %>%
    group_by(logger) %>%
    summarise(pearson_r = cor(measured, simulated, use = "complete.obs"),
              .groups = 'drop')
  
  # Calculate overall Pearson correlation coefficient
  cor_overall <- cor(df$measured, df$simulated, use = "complete.obs")
  
  # Print the correlations
  print(paste("Overall Pearson correlation for", dataset_name, ":", cor_overall))
  
  # Save the correlation data
  write_csv(correlations_by_sensor, paste0(dataset_name, "_correlations_by_sensor.csv"))
  write_csv(correlations_by_logger, paste0(dataset_name, "_correlations_by_logger.csv"))
  
  # Generate the plot
  plot <- ggplot(df, aes(x= measured, y=simulated)) +
    geom_point(alpha=0.5, color ="#1B9E77") +
    geom_smooth(method = "lm", color="black", alpha=0.5, size=0.7) +
    theme_minimal() +
    geom_abline(linetype="dotted") +
    ggtitle(paste(dataset_name,"r = ",paste(round(cor_overall, digits = 2))))+
    theme(plot.title = element_text(size=10),
          axis.title.x = element_text(size=09),
          axis.title.y = element_text(size=9))
  
  
  if (y_label) {
    plot <- plot + ylab(expression(paste("Sim. PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")))
  } else {
    plot <- plot + ylab("")
  }
  
  if (x_label) {
    plot <- plot + xlab(expression(paste("Meas. PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")))
  } else {
    plot <- plot + xlab("")
  }
  

 
  # Return correlation data
  return(list(
    correlations_by_sensor = correlations_by_sensor,
    correlations_by_logger = correlations_by_logger,
    cor_overall = cor_overall,
    plot
  ))
}

# Apply the function to each dataset and save the results
cor_results_ex <- calculate_and_plot(df_wide_ex, "a)", y_label = T)
cor_results_vox02 <- calculate_and_plot(df_wide_vox02, "b)", x_label = T)
cor_results_vox01 <- calculate_and_plot(df_wide_vox01, "c)")

ggarrange(cor_results_ex[[4]],
          cor_results_vox02[[4]],
          cor_results_vox01[[4]],
          nrow = 1)

# Calculate the mean, standard deviation, maximum, and minimum for each correlation result
mean_ex <- mean(cor_results_ex[[1]]$pearson_r, na.rm = TRUE)
sd_ex <- sd(cor_results_ex[[1]]$pearson_r, na.rm = TRUE)
max_ex <- max(cor_results_ex[[1]]$pearson_r, na.rm = TRUE)
min_ex <- min(cor_results_ex[[1]]$pearson_r, na.rm = TRUE)

mean_vox02 <- mean(cor_results_vox02[[1]]$pearson_r, na.rm = TRUE)
sd_vox02 <- sd(cor_results_vox02[[1]]$pearson_r, na.rm = TRUE)
max_vox02 <- max(cor_results_vox02[[1]]$pearson_r, na.rm = TRUE)
min_vox02 <- min(cor_results_vox02[[1]]$pearson_r, na.rm = TRUE)

mean_vox01 <- mean(cor_results_vox01[[1]]$pearson_r, na.rm = TRUE)
sd_vox01 <- sd(cor_results_vox01[[1]]$pearson_r, na.rm = TRUE)
max_vox01 <- max(cor_results_vox01[[1]]$pearson_r, na.rm = TRUE)
min_vox01 <- min(cor_results_vox01[[1]]$pearson_r, na.rm = TRUE)

# Create a data frame with the results
results_df <- data.frame(
  Metric = c("Mean", "Standard Deviation", "Maximum", "Minimum"),
  Explicit = c(mean_ex, sd_ex, max_ex, min_ex),
  Voxel_0.2m = c(mean_vox02, sd_vox02, max_vox02, min_vox02),
  Voxel_1m = c(mean_vox01, sd_vox01, max_vox01, min_vox01)
)



