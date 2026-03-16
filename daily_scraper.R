library(tidyverse)
library(gbfs)

# Fetch functions (simplified version of your dashboard logic)
fetch_summary <- function(slug, label) {
  tryCatch({
    base <- paste0("https://beryl-gbfs-production.web.app/v2_2/", slug, "/")
    info   <- gbfs::get_station_information(paste0(base, "station_information.json"))
    status <- gbfs::get_station_status(paste0(base, "station_status.json"))
    
    combined <- info %>%
      left_join(status, by = "station_id") %>%
      filter(!str_detect(toupper(name), "DEPOT|TEMP|TEMPORARY"))
    
    tibble(
      date = Sys.Date(),
      city = label,
      total_bikes = sum(combined$num_bikes_available, na.rm = TRUE),
      active_stations = sum(combined$is_renting, na.rm = TRUE),
      total_capacity = sum(combined$capacity, na.rm = TRUE)
    )
  }, error = function(e) return(NULL))
}

# Pull data
new_data <- bind_rows(
  fetch_summary("Leeds", "Leeds"),
  fetch_summary("Bradford", "Bradford")
)

# Append to CSV
file_path <- "data/beryl_history.csv"

if (file.exists(file_path)) {
  old_data <- read_csv(file_path)
  # Prevent duplicate entries for the same day if script runs twice
  final_data <- bind_rows(old_data, new_data) %>% distinct(date, city, .keep_all = TRUE)
} else {
  final_data <- new_data
}

write_csv(final_data, file_path)