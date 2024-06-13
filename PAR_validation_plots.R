library(tidyverse)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

# Read the wide data to calculate correlations
df_wide <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide.csv")

# Calculate Pearson correlation coefficient for each sensor
correlations_by_sensor <- df_wide %>%
  group_by(logger, sensor) %>%
  summarise(pearson_r = cor(measured, simulated, use = "complete.obs"),
            .groups = 'drop')

# Calculate Pearson correlation coefficient for each logger
correlations_by_logger <- df_wide %>%
  group_by(logger) %>%
  summarise(pearson_r = cor(measured, simulated, use = "complete.obs"),
            .groups = 'drop')

cor_overall <- cor(df_wide$measured, df_wide$simulated, use = "complete.obs")

ggplot(df_wide, aes(x= measured, y=simulated))+
  geom_point(alpha=0.5, color ="#1B9E77")+
  geom_smooth(method = "lm", color="black", alpha=0.5, size=0.7)+
  theme_minimal()+
  geom_abline(linetype="dotted")+
  ylab(expression(paste("Simulated PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")))+
  xlab(expression(paste("Measured PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")))
  

# Estimate the density
density_est <- density(df_wide$measured %>% na.omit())

# Find local minima in the density
find_local_minima <- function(density_est) {
  y <- density_est$y
  x <- density_est$x
  minima <- which(diff(sign(diff(y))) == 2) + 1
  data.frame(x = x[minima], y = y[minima])
}

local_min<-find_local_minima(density_est)

df_wide %>% 
  filter(measured <= local_min[1,1]) %>% 
  summarise(pearson_r = cor(measured, simulated, use = "complete.obs"))
  
df_wide %>% 
  filter(measured > local_min[1,1] & measured < local_min[2,1] ) %>% 
  summarise(pearson_r = cor(measured, simulated, use = "complete.obs"))

df_wide %>% 
  filter( measured >= local_min[2,1]) %>% 
  summarise(pearson_r = cor(measured, simulated, use = "complete.obs"))


t<-df_wide %>% 
  select(simulated,time) %>% 
  na.omit()







# Read the long data
df_par <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_long.csv")

# Filter to keep only timesteps with both measured and simulated values
df_filtered <- df_par %>%
  spread(key = type, value = umolm2s) %>%
  filter(!is.na(measured) & !is.na(simulated)) %>%
  gather(key = type, value = umolm2s, measured, simulated)

# Merge the correlation data with the filtered data
df_filtered <- df_filtered %>%
  left_join(correlations_by_logger, by = "logger")

# Function to create a single plot for each logger
create_logger_plot <- function(df, logger_num) {
  logger_data <- df %>% filter(logger == logger_num)
  correlation <- unique(logger_data$pearson_r)
  
  plot <- ggplot(logger_data, aes(time, umolm2s, color = type)) +
    geom_smooth(method = "loess", se = FALSE, span = 0.2, alpha = 0.7, size=0.7) +  # Add loess smoothing
    facet_wrap(~sensor) +
    ggtitle(paste("Logger", logger_num, " - Pearson R:", round(correlation, 2))) +
    theme(axis.text = element_text(size = 7)) +
    labs(
      x = NULL, 
      y = expression(paste("PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")"))
    ) +
    theme_classic() +
    theme(panel.grid.major = element_line(color = "grey", size = 0.1),
          panel.grid.minor = element_line(color = "lightgrey", size = 0.1),
          axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 8),
          legend.position = "none",
          legend.title = element_blank(),
          title = element_text(size = 8),
          strip.text = element_text(size = 7, margin = margin(2,2,2,2))) +  # Make facet strip box smaller
    ylim(c(0, 500)) +
    scale_y_continuous(breaks = seq(0, 400, by = 150))+
    scale_x_datetime(date_breaks = "1 hours", date_labels = "%H")+
    scale_color_brewer(palette = "Dark2", labels = c("Simulated", "Measured"))+
    ylab("")
  
  return(plot)
}

# Create plots for all loggers
logger_plots <- lapply(1:max(df_filtered$logger, na.rm = TRUE), function(logger_num) create_logger_plot(df_filtered, logger_num))

# Arrange plots
log_plot_arrange <- do.call(gridExtra::grid.arrange, c(logger_plots, ncol = 2))

# Add common y-axis label
annotate_figure(
  log_plot_arrange,
  left = text_grob(expression(paste("PAR (", mu, "mol ", m^{-2}, " ", s^{-1}, ")")), rot = 90, vjust = 1.5, hjust = 0.5, size=10),
  bottom = text_grob("Hour of Day", vjust = 0.5, hjust = 0.5, size=10)
)


val_dat$time %>% range
range()