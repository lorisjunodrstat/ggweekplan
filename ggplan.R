library(ggplot2)
library(dplyr)
library(lubridate)

planning <- data.frame(Jour = c("Lundi", "Mardi", "Vendredi"),
                       StartJour = c("2024-07-22 00:00:01", "2024-07-23 00:00:01", "2024-07-26 00:00:01"),
                       EndTJour = c("2024-07-22 23:59:59", "2024-07-24 23:59:59", "2024-07-26 23:59:59"),
                      Start = c("8:00", "12:00", "11:00"),
                      End = c("10:00", "15:00", "18:00"),
                      Cours = c("Math", "Biologie", "Statistique \nEuclidienne")
                      
                      )

str(planning)
planning$StartJour <- ymd_hms(planning$StartJour)
planning$EndTJour <- ymd_hms(planning$EndTJour)
planning$Start <- hm(planning$Start)
planning$End <- hm(planning$End)
library(scales)
reverse_time_trans <- function() {
  trans_new(name = "reverse_time",
    transform = function(x) -as.numeric(x),
    inverse = function(x) as.POSIXct(-x, origin = "1970-01-01"),
    breaks = function(x) pretty_breaks()(x),
    format = function(x) format(as.POSIXct(x, origin = "1970-01-01"), "%H:%M:%S")
  )
}

options(scipen = 10)
ggplot(planning) + 
  geom_rect(aes(xmin = StartJour, ymin = Start, xmax = EndTJour, ymax = End)) + 
  scale_x_datetime(labels = date_format("%a", locale = "fr_FR"), date_breaks = "1 day", position = "top") +
  scale_y_reverse(labels = function(x) as_hms(x),
                  breaks = seq(as.numeric(as_hms("08:00:00")), 
                               as.numeric(as_hms("18:30:00")),
                               7200))+ theme_pomological() + labs(title="Planning Semestre du Bachelor en sciences des données") +
  theme(panel.grid.major = element_line(colour = "yellow"),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(hjust = -2, size = 14, face = "bold", colour = "black"),
        title = element_text(size = 18, face="bold", colour = "black")
        
        
        
        ) + scale_color_pomological()
        
paint_pomological()          
                  
                  
                  
max(planning$End)
min(planning$Start)
planning_df
max()
  + theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+labs(x = "Jour de la semaine", y = "Heure") 
                                                                                                                                                             limits = c(min(data$date), max(data$date))
  , minor_breaks = "1 day", breaks = c(1,2,3,4,5))
     
class(planning$StartJour)                                                                                                             
info_locales()                                                                                    
library(gt)
fmt_time(
  planning,
  columns = everything(),
  rows = everything(),
  time_style = "iso",
  pattern = "{x}",
  locale = "fr"
)
?ymd_hm
str(planning)
planning
st_t <- ymd_hms("2024-07-26 00:00:00")
st_t
class(st_t)
View(st_t)
plot_weekly_schedule <- function(schedule_df) #{
  # Convert StartTime and EndTime to POSIXct objects
  planning_df <- planning %>%
    mutate(StartJour = ymd_hm(StartJour))
           EndTJour = ymd_hm("2024-01-01 23:59", EndTJour),
           Start =ymd_hm(paste("2024-01-01 00:00", Start)),
           End = ymd_hm(paste("2024-01-01 00:00", Start)))
  
  # Ensure the days are in order
  schedule_df$Day <- factor(schedule_df$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
  
  # Convert StartTime and EndTime to numeric for plotting
  schedule_df <- schedule_df %>%
    mutate(StartTime_numeric = as.numeric(StartTime),
           EndTime_numeric = as.numeric(EndTime),
           MidTime_numeric = (StartTime_numeric + EndTime_numeric) / 2)
  
  # Gather all unique time points for breaks and labels
  time_points <- sort(unique(c(schedule_df$StartTime_numeric, schedule_df$EndTime_numeric)))
  time_labels <- format(as.POSIXct(time_points, origin = "1970-01-01"), "%H:%M")
  
  # Plot using ggplot2
  ggplot(schedule_df, aes(x = Day, y = StartTime_numeric, fill = Course)) +
    geom_tile(aes(ymin = StartTime_numeric, ymax = EndTime_numeric), color = "white", size = 0.5) +
    geom_text(aes(y = MidTime_numeric, label = Course), color = "black", size = 3) +
    scale_y_continuous(breaks = time_points, labels = time_labels, expand = c(0, 0)) +
    scale_x_discrete() +
    scale_fill_brewer(palette = "Paired") +
    theme_minimal() +
    labs(title = "Weekly Course Schedule", x = "Day", y = "Time") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 8))  # Adjust y-axis text size if needed
}



# Call the function to plot the schedule
# Call the function to plot the schedule
plot_weekly_schedule(schedule_df)

