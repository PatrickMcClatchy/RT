# Read the wide data to calculate correlations
df_wide_ex <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide.csv")
df_wide_vox02 <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide_vox02.csv")
df_wide_vox01 <- read_csv("/Users/patrick/Desktop/MASTER THESIS/DART_PAR_VAL_wide_vox01.csv")


# Load necessary libraries
library(dplyr)
library(caret)

# Define the classification function
classify_par <- function(par_value, par_min, par_max) {
  if (par_value >= par_max - 200) {
    return("sunlit")
  } else if (par_value <= par_min + 75) {
    return("shaded")
  } else {
    return("half-shaded")
  }
}

# Assuming df_wide_vox02 is your data frame
# Assuming df_wide_vox02 is your data frame

data <- list(df_wide_ex,df_wide_vox02,df_wide_vox01)
data_class <- list()
for (i in 1:length(data)) {
  data_class[[i]] <- as.data.frame(data[[i]]) %>%
    na.omit() %>%
    group_by(time) %>%  # Group by the time column
    mutate(
      measured_min = min(measured, na.rm = TRUE),
      measured_max = max(measured, na.rm = TRUE),
      simulated_min = min(simulated, na.rm = TRUE),
      simulated_max = max(simulated, na.rm = TRUE)
    ) %>%
    rowwise() %>%  # Apply rowwise to ensure row-wise operations
    mutate(
      measured_class = classify_par(measured, measured_min, measured_max),
      simulated_class = classify_par(simulated, simulated_min, simulated_max)
    ) %>%
    ungroup() %>%
    select(-measured_min, -measured_max, -simulated_min, -simulated_max)  # Remove temporary columns if not needed
  
}

for (i in 1:length(data_class)) {
  
  # Set factor levels for ordering
  data_class[[i]]$measured_class <- factor(data_class[[i]]$measured_class, levels = c("shaded", "half-shaded", "sunlit"))
  data_class[[i]]$simulated_class <- factor(data_class[[i]]$simulated_class, levels = c("shaded", "half-shaded", "sunlit"))
  
  # Create the confusion matrix
  cm <- table(data_class[[i]]$measured_class, data_class[[i]]$simulated_class)
  
  # Convert the table to a data frame for ggplot2
  cm_df <- as.data.frame(cm)
  
  # Calculate the fraction of reference (per class)
  cm_df <- cm_df %>%
    group_by(Var1) %>%
    mutate(Fraction = Freq / sum(Freq) * 100) %>%
    ungroup()
  
  data_class[[i]] <- cm_df %>% 
    mutate(Fraction = round(Fraction))
  
}

conf_mat_list <- data_class

classification_summary <- list()

for (i in 1:length(data_class)) {
  classification_summary[[i]] <- data_class[[i]] %>%
    group_by(measured_class) %>%
    summarise(count = n(), proportion = n() / nrow(data_class[[i]]) * 100)
  
  # Print the summary
  cat("\nSummary for Dataset", i, ":\n")
  print(classification_summary[[i]])
}

# Plot the confusion matrix
cm_ex<-ggplot(conf_mat_list[[1]], aes(x = Var1, y = Var2, fill = Fraction)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%d\n%.0f%%", Freq, Fraction)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#1B9E77") +
  theme_minimal() +
  labs(x = "", y = "Prediction", fill = "Fraction of Reference (%)",
       subtitle = "a)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        axis.title.y = element_text(size=10),
        plot.subtitle = element_text(size = 11))

cm_v02<-ggplot(conf_mat_list[[2]], aes(x = Var1, y = Var2, fill = Fraction)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%d\n%.0f%%", Freq, round(Fraction, digits = 0))), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#1B9E77") +
  theme_minimal() +
  labs(x = "Reference", y = "Prediction (n = total number)", fill = "Fraction of Reference (%)",
       subtitle = "b)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        axis.title.x = element_text(size=10),
        plot.subtitle = element_text(size = 11))

cm_v1<-ggplot(conf_mat_list[[3]], aes(x = Var1, y = Var2, fill = Fraction)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%d\n%.0f%%", Freq, Fraction)), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "#1B9E77") +
  theme_minimal() +
  labs(x = "", y = "Prediction (n = total number)", fill = "Fraction of Reference (%)",
       subtitle = "c)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 8), legend.title = element_text(size = 9),
        plot.subtitle = element_text(size = 11))


ggarrange(cm_ex,cm_v02,cm_v1, nrow = 1, common.legend = TRUE, legend = "bottom")


