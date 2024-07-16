# Load required libraries
library(tidyverse)
library(terra)

# Set paths
simulation_path <- "/Volumes/PMSSD_work/Downloads/PAR_SENSOR_SIMULATION_EXPLICIT_FINAL"
sensor_metadata_path <- "/Users/patrick/Desktop/MASTER THESIS/label_x_y_z_pcrs.csv"
validation_data_path <- "/Users/patrick/Desktop/MASTER THESIS/par_clean.csv"
out_folder <- "/Users/patrick/Desktop/MASTER THESIS"

nadir_image_size <- 100
sensor_image_size <- 1

# Function to convert PAR surface irradiance to photon flux
convert_irradiance_to_photon_flux <- function(irradiance) {
  h <- 6.626e-34  # Planck's constant (J·s)
  c <- 3e8        # Speed of light (m/s)
  avogadro_number <- 6.022e23  # Avogadro's number (photons/mol)
  lambda_avg <- 550e-9  # Average wavelength of PAR in meters (550 nm)
  
  energy_per_photon <- h * c / lambda_avg  # J/photon
  photon_flux <- irradiance / energy_per_photon  # photons/m^2/s
  photon_flux_umol <- (photon_flux / avogadro_number) * 1e6  # µmol/m^2/s
  
  return(photon_flux_umol)
}

# Function to read sensor data
readSensor <- function(path, size = 1, raster = TRUE, plot = TRUE) {
  mat <- readBin(path, 'double', n = size * size) %>% matrix(nrow = size)
  rast <- terra::rast(mat)
  return(if (raster) rast else mat)
}

# Function to find paths of all sensor images in the simulation
find_sensor_images <- function(simulation_folder) {
  output_folders <- list.dirs(simulation_folder, full.names = TRUE, recursive = TRUE)
  raw_file_paths <- list()
  pattern <- "^(\\d+_\\d+)\\.mp#$"
  
  for (output_folder in output_folders) {
    subfolder_path <- file.path(output_folder, "BAND0/Radiance/ITERX/IMAGES_DART")
    if (dir.exists(subfolder_path)) {
      files <- list.files(subfolder_path, full.names = TRUE)
      raw_files <- files[grep(pattern, basename(files))]
      raw_file_paths <- c(raw_file_paths, raw_files)
    }
  }
  return(raw_file_paths)
}

# Function to find paths of all nadir images in the simulation
find_nadir_images <- function(simulation_folder) {
  output_folders <- list.dirs(simulation_folder, full.names = TRUE, recursive = TRUE)
  raw_file_paths <- list()
  pattern <- "ima001_VZ=000_0_VA=000_0.mp#"
  
  for (output_folder in output_folders) {
    subfolder_path <- file.path(output_folder, "BAND0/Radiance/ITERX/IMAGES_DART")
    if (dir.exists(subfolder_path)) {
      files <- list.files(subfolder_path, full.names = TRUE)
      raw_files <- files[grep(pattern, basename(files))]
      raw_file_paths <- c(raw_file_paths, raw_files)
    }
  }
  return(raw_file_paths)
}

# Function to plot nadir images
plot_nadir_images <- function(nadir_image_files, size = 100, title = "Nadir plot") {
  timestamps <- unlist(lapply(nadir_image_files, function(path) {
    sub(".*/output_(\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2})/.*", "\\1", path)
  }))
  
  if (length(nadir_image_files) == 0) stop("No sensor files found for the specified sensor name.")
  
  df_list <- lapply(1:length(nadir_image_files), function(i) {
    sensor_dat <- readSensor(nadir_image_files[[i]], size = size)
    timestamp <- timestamps[i]
    data.frame(x = rep(1:size, each = size), y = rep(1:size, times = size), z = as.vector(sensor_dat), timestamp = timestamp)
  })
  
  df <- do.call(rbind, df_list)
  
  p <- ggplot(df, aes(x = x, y = y, fill = z)) +
    geom_tile() +
    facet_wrap(~ timestamp, ncol = round(sqrt(length(nadir_image_files)))) +
    scale_fill_continuous(name = "Radiance") +
    theme_minimal() +
    scale_y_reverse() +
    ggtitle(title)
  
  print(p)
  return(p)
}


################################################################################

# Read validation data
val_dat <- read_csv(validation_data_path) %>%
  rename(measured = "par_umolm2s") %>%
  rename(sensor = "channel") %>% 
  dplyr::select(logger, sensor, time_utc, measured) %>%
  rename(time = "time_utc") %>%
  distinct() %>%
  mutate(logger = as.character(logger)) %>%
  mutate(sensor = as.character(sensor)) %>% 
  mutate(time = if_else(logger == "2", time - seconds(30), time))


sensor_metadata <- read_csv(sensor_metadata_path)

# Find nadir image paths and plot them
nadir_image_paths <- find_nadir_images(simulation_path)[-1]
plot_nadir_images(nadir_image_paths, nadir_image_size, "Nadir Radiance Images")

# Extract irradiance values from sensors
par_sensor_image_paths <- find_sensor_images(simulation_path)
mean_value_sensor_images <- data.frame(timestamp = character(), mean_sensor_value = numeric(), sensor_name = character(), stringsAsFactors = FALSE)
sensor_names <- unique(sensor_metadata$Name)

# Loop through each sensor
for (sensor in sensor_names) {
  sensor_image_data <- plot_sensor_data(sensor, par_sensor_image_paths, size = sensor_image_size)
  df <- data.frame(timestamp = sub("/.*", "", names(sensor_image_data$mean_df)), mean_sensor_value = as.numeric(sensor_image_data$mean_df), sensor_name = sensor, stringsAsFactors = FALSE)
  mean_value_sensor_images <- rbind(mean_value_sensor_images, df)
}

mvsi <- mean_value_sensor_images %>%
  filter(timestamp != "C:") %>%
  separate(sensor_name, into = c("logger", "sensor"), sep = "_") %>%
  mutate(timestamp = as_datetime(timestamp)) %>%
  rename("simulated" = mean_sensor_value) %>% 
  as_tibble()

# Join data and save to CSV
df_join <- full_join(val_dat, mvsi, by = c("logger", "sensor", "time" = "timestamp")) %>%
  mutate(measured = measured) %>%
  mutate(simulated = convert_irradiance_to_photon_flux(simulated))%>%
  pivot_longer(cols = c("simulated", "measured"), names_to = "type", values_to = "umolm2s") %>% 
  na.omit() %>% 
  mutate(time_local = time + hours(2))

df_join_wide <- full_join(val_dat, mvsi, by = c("logger", "sensor", "time" = "timestamp")) %>% 
  mutate(simulated = convert_irradiance_to_photon_flux(simulated)) %>% 
  mutate(time_local = time + hours(2))

write_csv(df_join, file.path(out_folder, "DART_PAR_VAL_long_vox01.csv"))
write_csv(df_join_wide, file.path(out_folder,"DART_PAR_VAL_wide_vox01.csv"))


