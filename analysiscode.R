url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists("rawdata.zip")){
    download.file(url = url,
                  destfile = "rawdata.zip")
}
unzip(zipfile = "rawdata.zip", 
      exdir = "rawdata")