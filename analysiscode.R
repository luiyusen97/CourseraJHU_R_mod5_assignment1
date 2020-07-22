# loading packages
library(tidyverse)

# file downloading DATA PROCESSING
url <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
         "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
         "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf")
if (!file.exists("rawdata.csv.bz2")){
    download.file(url = url[1],
                  destfile = "rawdata.csv.bz2")
}
if (!file.exists("rawdata_doc.pdf")){
    download.file(url = url[2],
                  destfile = "rawdata_doc.doc")
}
if (!file.exists("rawdata_faq.pdf")){
    download.file(url = url[3],
                  destfile = "rawdata_faq.pdf")
}

#read data
stormdata <- read.csv("rawdata.csv.bz2")

# harmful wrt population hp FATALITIES/INJURIES

stormdata_orderedevtype <- group_by(stormdata, EVTYPE, .drop = FALSE)
sum_fatalities <- summarise(stormdata_orderedevtype, sum_fat = sum(FATALITIES))
sum_injuries <- summarise(stormdata_orderedevtype, sum_inj = sum(INJURIES))
sum_popdmg <- merge(sum_fatalities, sum_injuries, by = "EVTYPE")
# i <- 1
# while (TRUE){
#     pat <- sum_popdmg[i, 1]
#     match_indices <- agrep(pattern = pat,
#                            x = sum_popdmg[ , 1],
#                            ignore.case = TRUE)
#     match_values <- sum_popdmg[match_indices, ]
#     new_value <- list(match_values[1, 1],
#                       sum(match_values[ , 2]),
#                       sum(match_values[ , 3]))
#     sum_popdmg[i, 2] <- new_value[2] ; sum_popdmg[i, 3] <- new_value[3]
#     sum_popdmg <- sum_popdmg[-(match_indices[2:length(match_indices)]), ]
#     i <- i + 1
#     if (i > nrow(sum_popdmg)){
#         break
#     }
# }

empty_indices <- vector(mode = "numeric")
for (i in 1:nrow(sum_popdmg)){
    if ((sum_popdmg[i, 2] < 1000) & (sum_popdmg[i, 3] < 1000)){
        empty_indices <- c(empty_indices, i)
    }
}
sum_popdmg <- sum_popdmg[-empty_indices, ]
sum_popdmg <- mutate(sum_popdmg, eventtype = 1:nrow(sum_popdmg))

max_fat <- sum_popdmg[which(sum_popdmg$sum_fat==max(sum_popdmg$sum_fat)), ]
max_inj <- sum_popdmg[which(sum_popdmg$sum_inj==max(sum_popdmg$sum_inj)), ]

sum_popdmg_plot <- ggplot(sum_popdmg) +
    geom_point(mapping = aes(x = eventtype, y = sum_fat, colour = "red")) + 
    geom_point(mapping = aes(x = eventtype, y = sum_inj, colour = "blue")) +
    geom_point(shape = 1, mapping = aes(x = max_fat[ , 4], y = max_fat[ , 2])) +
    geom_point(shape = 1, mapping = aes(x = max_inj[ , 4], y = max_inj[ , 3])) +
    labs(title = "Fatalities and Injuries by event type",
         x = "Event type", y = "Fatalities/Injuries") +
    scale_color_manual(labels = c("Fatalities", "Injuries"),
                       values = c("red", "cadetblue3"))
print(sum_popdmg_plot)
ggsave("pop_dmg.png", plot = sum_popdmg_plot, device = png())
dev.off()

# economic damage
sum_propdmg <- summarise(stormdata_orderedevtype, sum_propdmg = sum(PROPDMG))
sum_cropdmg <- summarise(stormdata_orderedevtype, sum_cropdmg = sum(CROPDMG))
sum_econdmg <- merge(sum_propdmg, sum_cropdmg, by = "EVTYPE")

empty_indices <- vector(mode = "numeric")
for (i in 1:nrow(sum_econdmg)){
    if ((sum_econdmg[i, 2] < 10000) & (sum_econdmg[i, 3] < 10000)){
        empty_indices <- c(empty_indices, i)
    }
}
sum_econdmg <- sum_econdmg[-empty_indices, ]
sum_econdmg <- mutate(sum_econdmg, eventtype = 1:nrow(sum_econdmg))

max_propdmg <- sum_econdmg[which(sum_econdmg$sum_propdmg==max(sum_econdmg$sum_propdmg)), ]
max_cropdmg <- sum_econdmg[which(sum_econdmg$sum_cropdmg==max(sum_econdmg$sum_cropdmg)), ]

sum_econdmg_plot <- ggplot(sum_econdmg) +
    geom_point(mapping = aes(x = eventtype, y = sum_propdmg, colour = "red")) + 
    geom_point(mapping = aes(x = eventtype, y = sum_cropdmg, colour = "blue")) +
    geom_point(shape = 1, mapping = aes(x = max_propdmg[ , 4], y = max_propdmg[ , 2])) +
    geom_point(shape = 1, mapping = aes(x = max_cropdmg[ , 4], y = max_cropdmg[ , 3])) +
    labs(title = "Property and crop damage by event type",
         x = "Event type", y = "Property/Crop damage") +
    scale_color_manual(labels = c("Property damage", "Crop damage"),
                       values = c("red", "cadetblue3"))
print(sum_econdmg_plot)
ggsave("econ_dmg.png", plot = sum_econdmg_plot, device = png())
dev.off()