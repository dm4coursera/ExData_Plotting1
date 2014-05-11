plot4 <- function() {
  # This function creates "plot4.png" for the Peer Reviewed project 1 assignment
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
  png(filename="plot4.png", width=480, height=480)
  
  # The charts are displayed in a 2x2 configuration
  par(mfrow=c(2,2))
  
  # Small chart number 1
  plot(energy$timestamp, energy$Global_active_power, type="l", ylab="Global Active Power", xlab="")
  
  # Small chart number 2
  plot(energy$timestamp, energy$Voltage, type="l", ylab="Voltage", xlab="datetime")
  
  # Small chart number 3
  plot(energy$timestamp, energy$Sub_metering_1,xlab="",ylab="Energy sub metering",col="black", type="l")
  lines(energy$timestamp, energy$Sub_metering_2, col="red", type="l")
  lines(energy$timestamp, energy$Sub_metering_3, col="blue", type="l")
  legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), lty=c(1,1), bty="n", col=c("black","red","blue"))
  
  # Small chart number 4
  plot(energy$timestamp, energy$Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime")
  
  dev.off()
}