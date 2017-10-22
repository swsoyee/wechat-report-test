msgPerDay <- dt %>%
    select(CreateTime, Type) %>%
    mutate(Year = year(CreateTime), Month = month(CreateTime), Day = day(CreateTime)) %>%
    group_by(Year, Month, Day) %>%
    summarise(Count = n())

# Data convert
data <- spread(msgPerDay, key = Day, value = Count)
data <- as.data.frame(data)
data[is.na(data)] <- 0
rownames(data) <- paste0(data$Year, "年", data$Month, "月")
data <- as.matrix(data[,3:ncol(data)])
#data <- data[,3:ncol(data)]

# Color scale
vals <- unique(scales::rescale(c(data)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)
colz[, 2] <- as.character(colz[,2])
colz[colz[,1] == 0, 2] <- "#FFFFFF"
colz[, 2] <- as.factor(colz[,2])

# Hover text
data.text <- character(0)
for (i in 1:nrow(data)) {
    tmp <- sapply(data[i, ], function(x) paste0("日</br></br>收发消息:", x))
    tmp <- paste0(rownames(data)[i], colnames(data), tmp)
    data.text <- append(data.text, tmp)
}
data.text <- matrix(data.text, ncol = 31, byrow = TRUE)

# Weekend
data.week <- character(0)
for (i in 1:nrow(data)) {
    tmp <- paste0(rownames(data)[i], colnames(data))
    data.week <- append(data.week, tmp)
}
data.week <- str_replace_all(string = data.week, 
                                pattern = "年|月", 
                                replacement = "-")
data.week <- as.Date(data.week)
data.week <- weekdays(data.week)
data.week <- matrix(data.week, ncol = 31, byrow = TRUE)

# Annotation
position.zero <- which(data == 0, TRUE)
position.sat <- which(data.week == c("星期六"), TRUE)
position.sun <- which(data.week == c("星期日"), TRUE)

# Plot
plot_ly(x = colnames(data),
        y = rownames(data),
        z = data,
        height = 40*nrow(data),
        showlegend = FALSE,
        colorscale = colz,
        type = "heatmap",
        hoverinfo = "text",
        text = data.text) %>%
    layout(title = "消息数热图",
           xaxis = list(title = "", showgrid = FALSE),
           yaxis = list(showgrid = FALSE),
           margin = list(t = 30, l = 80)) %>%
    add_annotations(
        x = position.sat[, 2],
        y = position.sat[, 1]-1,
        text = "○",
        font = list(color = "#08306B", size = 16),
        xanchor = 'center',
        showarrow = F
    ) %>%
    add_annotations(
        x = position.sun[, 2],
        y = position.sun[, 1]-1,
        text = "●",
        font = list(color = "#08306B", size = 16),
        xanchor = 'center',
        showarrow = F
    )
    
