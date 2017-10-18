msgPerDay <- dt %>%
    select(CreateTime, Type) %>%
    mutate(Year = year(CreateTime), Month = month(CreateTime), Day = day(CreateTime)) %>%
    group_by(Year, Month, Day) %>%
    summarise(Count = n())

# Color scale
vals <- unique(scales::rescale(c(data)))
o <- order(vals, decreasing = FALSE)
cols <- scales::col_numeric("Blues", domain = NULL)(vals)
colz <- setNames(data.frame(vals[o], cols[o]), NULL)

# Data convert
data <- spread(msgPerDay, key = Day, value = Count)
data <- as.data.frame(data)
data[is.na(data)] <- 0
rownames(data) <- paste0(data$Year, "年", data$Month, "月")
#data <- as.matrix(data[,3:ncol(data)])
data <- data[,3:ncol(data)]

# Hover text
data.text <- paste(rownames(data), colnames(data), "日</br></br>收发消息:",data)
data.text <- matrix(data.text, ncol = 31, byrow = TRUE)
data.text <- lapply(data, function(x) paste(rownames(data), colnames(data), "日</br></br>收发消息:",data))
data.text <- matrix(data.text, ncol = 31, byrow = TRUE)

plot_ly(x = colnames(data),
        y = rownames(data),
        z = data,
        colorscale = colz,
        type = "heatmap",
        hoverinfo = "text",
        text = data.text) %>%
    layout(title = "消息数热图",
           xaxis = list(title = ""),
           margin = list(t = 30, b = 250, l = 80))
