plot1 <- function() {
  # This function creates "plot1.png" for the Peer Reviewed project 1 assignment
  # of Coursera - Exploratory Data Analysis.
  
  # Read the entire energy usage file and then extract the desired dates,
  # which for this project is the first two days of February 2007.
  #
  # Note: the date format in the file is DAY/MONTH/YEAR.
  # Note: the data is read "as.is" to prevent conversion into factor levels.
  df <- read.csv("household_power_consumption.txt", header=T, sep=";", as.is=TRUE)
  energy <- subset(df, df$Date=="1/2/2007" | df$Date == "2/2/2007")
  
  # The date and time columns are character strings, which won't be useful
  # when we need to construct plots that reference datetime.  So take those
  # two columns and create a new datetime column that uses the POSIX datetime class.
  energy$timestamp <- strptime(paste(energy$Date,energy$Time),format="%d/%m/%Y %H:%M:%S")

  # Make sure all of the numeric fields are really numerics.
  energy$Global_active_power <- as.numeric(energy$Global_active_power)
  energy$Global_reactive_power <- as.numeric(energy$Global_reactive_power)
  energy$Voltage <- as.numeric(energy$Voltage)
  energy$Global_intensity <- as.numeric(energy$Global_intensity)
  energy$Sub_metering_1 <- as.numeric(energy$Sub_metering_1)
  energy$Sub_metering_2 <- as.numeric(energy$Sub_metering_2)
  energy$Sub_metering_3 <- as.numeric(energy$Sub_metering_3)
  
  # Create a PNG graphics device that is 480x480.
  png(filename="plot1.png", width=480, height=480)
  
  # Create a PNG histogram that matches the assignment instructions
  hist(energy$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")
  dev.off()
}