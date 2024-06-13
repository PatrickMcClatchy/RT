# Install and load necessary packages
install.packages("lidR")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(lidR)
library(ggplot2)
library(RColorBrewer)

# Load a point cloud data file
point_cloud <- readLAS("/Users/patrick/Desktop/MASTER THESIS/tree_summer_1cm.las")

# Extract the reflectance values
reflectance_values <- point_cloud$Reflectance

# Create a data frame with reflectance values and a color category based on the threshold
data <- data.frame(
  reflectance_values = reflectance_values,
  category = ifelse(reflectance_values < -5.4, "Foliage", "Wood")
)

data %>% filter(category == "Wood") %>% nrow()


# Create a histogram with conditional coloring using ggplot2
histogram_plot <- ggplot(data, aes(x = reflectance_values, fill = category)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.6, position = "stack", size=0.3) +
  scale_fill_brewer(palette = "Dark2") +
  geom_vline(xintercept = -5.4, color = "red", linetype = "dashed", size = 0.8) +
  labs(x = "Reflectance",
       y = "Frequency",
       fill = "") +
  theme_minimal()+
  theme(axis.title.x = element_text(size=10),
        axis.title.y = element_text(size=10),
        legend.position = "top",
        legend.key.size = unit(0.5, "lines"))


histogram_plot
################################################################################
library(scales)
# Extract coordinates

# Extract coordinates and reflectance
point_data <- data.frame(X = point_cloud$X, Y = point_cloud$Y, Z = point_cloud$Z, Reflectance = point_cloud$Reflectance)

# Downsample the point cloud
set.seed(123)  # For reproducibility
downsampled_point_data <- point_data %>% sample_frac(0.1)

# Add a category based on reflectance threshold
downsampled_point_data <- downsampled_point_data %>%
  mutate(Category = ifelse(Reflectance < -5.4, "Foliage", "Wood")) %>% 
  mutate(X = X - min(X),
         Y = Y - min(Y),
         Z = Z - min(Z))

# Rescale Y for plotting purposes
downsampled_point_data$Scaled_Y <- rescale(downsampled_point_data$Y, to = c(0, 1))

# Plot the point cloud using ggplot2
point_cloud_plot <- ggplot(downsampled_point_data, aes(x = X, y = Z, color = Category, alpha = Scaled_Y)) +
  geom_point(size = 0.1) +
  labs(x = "X [m]",
       y = "Z [m]",
       color = "Category",
       alpha = "Scaled Y") +
  theme_minimal()+
  scale_color_brewer(palette = "Dark2", labels = c("Foliage", "Wood"))+
  theme(legend.position = "none")

point_cloud_plot

ggarrange(point_cloud_plot, histogram_plot)



