# Generate the plot1
plot1 <- function(init = TRUE) {
        png(filename = "plot1.png", width = 480, height = 480)
        if(init) {
                init()
        }
        data <- get_data()

        do_plot1(data)

        dev.off()
}

# Helper function for plot1
do_plot1 <- function(data) {
        hist(x = data$Global_active_power,
             col = "red",
             xlab = "Global Active Power (kilowatts)",
             main = "Global Active Power",
             freq = F)
}

# Generate the plot2
plot2 <- function(init = TRUE) {
        png(filename = "plot2.png", width = 480, height = 480)
        if(init) {
                init()
        }
        data <- get_data()

        do_plot2(data)

        dev.off()
}

# Helper function for plot2
do_plot2 <- function(data) {

        plot(x = data$Datetime,
             y = data$Global_active_power,
             xlab = weekdays(data$Datetime),
             ylab = "Global Active Power (kilowatts)",
             type = "l")
}


# Generate the plot3
plot3 <- function (init = TRUE) {
        png(filename = "plot3.png", width = 480, height = 480)
        if(init) {
                init()
        }
        data <- get_data()

        do_plot3(data)

        dev.off()
}

# Helper function for plot3
do_plot3 <- function(data) {
        xrange = data$Datetime
        yrange = data$Sub_metering_Value

        plot(xrange,
             yrange,
             xlab = weekdays(data$Datetime),
             ylab = "Energy sub metering",
             type = "n" )

        lines(y = data$Sub_metering_Value[data$Sub_metering_Index == 1],
              x = data$Datetime[data$Sub_metering_Index == 1],
              type = "l" )

        lines(y = data$Sub_metering_Value[data$Sub_metering_Index == 2],
              x = data$Datetime[data$Sub_metering_Index == 2],
              type = "l",
              col = "red")

        lines(y = data$Sub_metering_Value[data$Sub_metering_Index == 3],
              x = data$Datetime[data$Sub_metering_Index == 3],
              type = "l",
              col = "blue")

        legend("topright",
               "groups",
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               col = c('black', 'red', 'blue'),
               lwd=c(1, 1, 1))
}

# Generate the plot4
plot4 <- function(init = TRUE) {
        png(filename = "plot4.png", width = 480, height = 480)
        if(init) {
                init()
        }
        data <- get_data()

        do_plot4(data)

        dev.off()
}

# Helper function for plot4
do_plot4 <- function (data) {
        par(mfrow=c(2,2))

        # Plot2 at pos1
        do_plot2(data)

        # Pos 2
        plot(x = data$Datetime,
             y = data$Voltage,
             xlab = weekdays(data$Datetime),
             ylab = "Voltage",
             type = "l")

        #Pos 3
        do_plot3(data)

        # Pos 4
        plot(x = data$Datetime,
             y = data$Global_reactive_power,
             xlab = weekdays(data$Datetime),
             ylab = "Global reactive power",
             type = "l")
}

# Load required libraries
init <- function() {
        #install.packages("dplyr")
        #install.packages("tidyr")
        message("Initialising...")
        library(dplyr)
        library(tidyr)

        setClass('myDate')
        setClass('myTime')
        setAs("character","myDate", function(from) as.Date(from, format="%d/%m/%Y") )
        setAs("character","myTime", function(from) format(from, format="%H:%M:%S") )
}

# Return the tidied data to use for plotting
get_data <- function() {
        # Download the data
        if(!file.exists(file="household_power_consumption.zip")) {
                message("Download data file...")
                download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                              destfile = "household_power_consumption.zip",
                              method = "curl")
        }

        if(!file.exists(file="household_power_consumption.txt")) {
                message("Unzip data file...")
                unzip(zipfile = "household_power_consumption.zip")
        }

        # Read the data
        message("Load the data...")
        hhpc <- read.csv(file = "household_power_consumption.txt",
                       sep = ";",
                       colClasses=c("myDate", "myTime", rep("numeric", times=7)),
                       na.string = "?")

        # Tidy and filter the data
        message("Tidy and filter the data...")
        hhpc <- hhpc %>%
                filter(Date == as.Date('2007-02-01') | Date == as.Date('2007-02-02')) %>%
                mutate(Datetime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")) %>%
                mutate(Weekday = weekdays(Datetime)) %>%
                gather(Sub_metering_Index, Sub_metering_Value, Sub_metering_1:Sub_metering_2:Sub_metering_3) %>%
                mutate(Sub_metering_Index = gsub("Sub_metering_", "", Sub_metering_Index))

}
