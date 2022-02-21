library(data.table)
library(dplyr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(gridExtra)
library(corrplot)

setwd("C:/Users/matth/Desktop/Course Materials for Stu/AY21 Team Assignment and Project/Plant_1_Generation_Data")

#Generation Data
gen.dt <- fread("Plant_1_Generation_Data.csv", stringsAsFactors = F)

# Convert to datetime format
gen.dt$DATE = as.Date(gen.dt$DATE_TIME,format = '%d-%m-%Y')
dates <- as.POSIXct(gen.dt$DATE_TIME,format = '%d-%m-%Y %H:%M')
gen.dt$TIME <- format(dates, format = "%H:%M")
# Drop Date_Time, Add in Day of the week
gen.dt$DATE_TIME <- NULL
gen.dt$DOTW = weekdays(gen.dt$DATE)

inverters <- c(unique(gen.dt$SOURCE_KEY))
f_gen.dt <- data.table()
for ( i in c(1:22)){
  gen1.dt <- subset(gen.dt, SOURCE_KEY == inverters[i])
  gen2.dt <- as.data.table(mutate(gen1.dt, PERIOD_YIELD = gen1.dt$TOTAL_YIELD - lag(gen1.dt$TOTAL_YIELD)))
  gen2.dt <- gen2.dt[PERIOD_YIELD < 0, PERIOD_YIELD := 0]
  f_gen.dt <- rbind(f_gen.dt,gen2.dt)
}

#Weather Data
weather.dt <- fread("Plant_1_final_weather.csv", stringsAsFactors = F)
weather.dt$SOURCE_KEY <- NULL
weather.dt$PLANT_ID <- NULL
weather.dt$V1 <- NULL
names(weather.dt)[names(weather.dt) == "Date"] <- "DATE"
names(weather.dt)[names(weather.dt) == "Time"] <- "TIME"
weather.dt$DATE = as.Date(weather.dt$DATE,format = "%d/%m/%Y")
times <- as.POSIXct(weather.dt$TIME ,format = '%H:%M:%S')
weather.dt$TIME <- format(times, format = "%H:%M")

#Merged Dataset
plant_1.dt <- merge(f_gen.dt, weather.dt,by = c("DATE","TIME"), all.x = TRUE)
plant_1.dt <- na.omit(plant_1.dt)
plant_1.dt$MODULE_TEMPERATURE = round(plant_1.dt$MODULE_TEMPERATURE,digits=2)
plant_1.dt$AMBIENT_TEMPERATURE = round(plant_1.dt$AMBIENT_TEMPERATURE,digits=2)

#Exploratory Data Analysis
# Univariate Analysis
data=plant_1.dt%>% select(PERIOD_YIELD)
status(data)
#Histogram
plot_num(data)
#Period Yield across time
ggplot(aes(x = TIME, y = PERIOD_YIELD), data = plant_1.dt) + geom_point() + geom_point(color = 'red',stat = 'summary', fun = 'mean') + facet_wrap(~DATE)
#Find time intervals where Period Yield > 0
Zero_Yield.dt = subset(plant_1.dt, PERIOD_YIELD > 0)
ggplot(aes(x = TIME), data = Zero_Yield.dt) + geom_bar(fill = "#FF6666")

#Resolve Anomalies
# Anomaly 1 on 19-05-21: Missing 3 periods. Hence calculated Period Yield was cumulative over 4 period. Therefore, replace with value/4
new_values <- plant_1.dt[plant_1.dt$DATE == '2020-05-19' & plant_1.dt$TIME == '12:30']$PERIOD_YIELD
plant_1.dt[plant_1.dt$DATE == '2020-05-19' & plant_1.dt$TIME == '12:30']$PERIOD_YIELD <- new_values/4

#Anomaly 2 on 20-05-21: Missing 8 periods. However as timespan is too large to replace with an average, we will replace values at 1730 with the average of other days at that time
for ( i in inverters){
  new_value = mean(plant_1.dt[plant_1.dt$TIME == '17:30' & plant_1.dt$DATE != '2020-05-20' & plant_1.dt$SOURCE_KEY == i]$PERIOD_YIELD)
  plant_1.dt[plant_1.dt$TIME == '17:30' & plant_1.dt$DATE == '2020-05-20' & plant_1.dt$SOURCE_KEY == i]$PERIOD_YIELD <- new_value
}

# Anomaly 3 on 21-05-21: Missing 1 period. Hence calculated Period Yield was cumulative over 2 periods. Therefore, replace with value/2
new_values <- plant_1.dt[plant_1.dt$DATE == '2020-06-03' & plant_1.dt$TIME == '14:15']$PERIOD_YIELD
plant_1.dt[plant_1.dt$DATE == '2020-06-03' & plant_1.dt$TIME == '14:15']$PERIOD_YIELD <- new_values/2

#Filter data to between 0600 & 2015
plant_1_0600to2015.dt = subset(plant_1.dt, TIME < "20:15" & TIME > "06:00")

#Bivariate Analysis

# Plot of different weather variables against Period Yield
plot_1 <-ggplot(aes(y = MODULE_TEMPERATURE, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_2 <-ggplot(aes(y = AMBIENT_TEMPERATURE, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_3 <- ggplot(aes(y = IRRADIATION, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_4 <- ggplot(aes(y = `Relative Humidity (%)`, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_5 <- ggplot(aes(y = `Pressure (hPa)`, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_6 <- ggplot(aes(y = `Wind speed (m/s)`,x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_7 <- ggplot(aes(y = `Wind direction`, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");
plot_8 <- ggplot(aes(y = `Rainfall (kg/m2)`, x = PERIOD_YIELD), data = plant_1_0600to2015.dt) + geom_point()  + geom_smooth(method = "gam");

grid.arrange(plot_1, plot_2,plot_3, plot_4, ncol=2)
grid.arrange(plot_5, plot_6,plot_7, plot_8, ncol=2)

# Correlation Matrix
data = plant_1_0600to2015.dt%>% select(PERIOD_YIELD,MODULE_TEMPERATURE,AMBIENT_TEMPERATURE,IRRADIATION,`Relative Humidity (%)`,`Pressure (hPa)`,`Wind speed (m/s)`,`Wind direction`,`Rainfall (kg/m2)`)
M <- cor(data)
colnames(M) <- c("PY", "MT", "AT", "Irr", "RH",'Pa','WS','WD','RF')
corrplot.mixed(M)

#Save final dataset without scaling
write.csv(plant_1_0600to2015.dt,"Plant_1_Data.csv", row.names=FALSE)


#Scale variables
scaled_plant_1.dt <- plant_1_0600to2015.dt
colx.dt <- c('DC_POWER', 'AC_POWER', 'DAILY_YIELD' ,'TOTAL_YIELD', 'PERIOD_YIELD', 'AMBIENT_TEMPERATURE','MODULE_TEMPERATURE','IRRADIATION','Relative Humidity (%)','Pressure (hPa)', 'Wind speed (m/s)','Wind direction', 'Rainfall (kg/m2)' )
scaled_values <- scale(plant_1_0600to2015.dt[,colx.dt, with=FALSE], center = TRUE, scale = TRUE)
for (i in c(1:13)){
  scaled_plant_1.dt[,colx.dt[i]] <- scaled_values[,colx.dt[i]]
}
#View(scaled_plant_1.dt)

#How weather variables and Period Yield vary with time
#Ambient Temp
AT_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=AMBIENT_TEMPERATURE), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean') 
#Module Temp
MT_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=MODULE_TEMPERATURE), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')
#Irradiation
Irr_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=IRRADIATION), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')  
#Relative Humidity
RH_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=`Relative Humidity (%)`), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')
#Pressure
P_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=`Pressure (hPa)`), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')
#Wind Speed
WS_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=`Wind speed (m/s)`), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')
#Wind Direction
WD_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=`Wind direction`), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')
#Rainfall & Period Yield vs Time
RF_plot <- ggplot(scaled_plant_1.dt, aes(x=TIME)) + geom_point( aes(y=`Rainfall (kg/m2)`), color ='red',stat = 'summary', fun = 'mean') + geom_point( aes(y=PERIOD_YIELD), color ='blue',stat = 'summary', fun = 'mean')

grid.arrange(AT_plot, MT_plot,Irr_plot, RH_plot, ncol=2)
grid.arrange(P_plot, WS_plot,WD_plot, RF_plot, ncol=2)

#Final dataset to be used
write.csv(plant_1_0600to2015.dt,"Plant_1_Normalized_Data.csv", row.names=FALSE)
