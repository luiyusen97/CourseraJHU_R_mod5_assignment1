# loading packages
library(tidyverse)

# file downloading
url <- c("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
         "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf",
         "https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf")
if (!file.exists("rawdata.csv.bz2")){
    download.file(url = url[1],
                  destfile = "rawdata.csv.bz2")
}
if (!file.exists("rawdata_doc.pdf")){
    download.file(url = url[2],
                  destfile = "rawdata_doc.pdf")
}
if (!file.exists("rawdata_faq.pdf")){
    download.file(url = url[3],
                  destfile = "rawdata_faq.pdf")
}

#read data
stormdata <- read.csv("rawdata.csv.bz2")