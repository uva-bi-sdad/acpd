#date(x) and count(y) scatter plot
#color by the day of the week 

acpd <- read.csv("~/git/acpd/data/initial_filtering.csv")
pacman::p_load(docstring, sdalr, DBI, dplyr, data.table, dtplyr)



input <- relevant_crime_types[, c('date', 'count')]
png(filename = "crimescatterplot.png")

plot(x = input$date, y = input$count, type = "b"
    xlab = "Date",
    ylab = "Count"
)

?plot



