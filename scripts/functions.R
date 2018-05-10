library(plotly)

# Create axis lists
axis <- function(x)list(title = x)

# Plot a barchart with date on the horizontal axis
bartime <- function(data, x, y, split, barmode = "stack", title = "", 
                    xaxis = "Date", yaxis){
    p <- plot_ly(data, x = x, y = y, split = split, type = "bar") %>% 
        layout(barmode = barmode, title = title,
               xaxis = axis(xaxis), yaxis = axis(yaxis))
    return(p)
}