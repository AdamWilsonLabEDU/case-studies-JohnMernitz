library(tidyverse)
library(ggplot2)
dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

# the next lines download the data
temp=read_csv(dataurl, 
              na="999.90", # tell R that 999.90 means missing in this dataset
              skip=1, # we will use our own column names as below so we'll skip the first row
              col_names = c("YEAR","JAN","FEB","MAR", # define column names 
                            "APR","MAY","JUN","JUL",  
                            "AUG","SEP","OCT","NOV",  
                            "DEC","DJF","MAM","JJA",  
                            "SON","metANN"))
# renaming is necessary because they used dashes ("-")
# in the column names and R doesn't like that.
head(temp)

ggplot(temp, aes(x = YEAR, y = JJA), stat = 'mean') +
  geom_line(color = "black") +
  geom_smooth(method = "loess", color = 'red') +
  theme_classic() +
  xlab("Year") + 
  ylab("Mean Summer Temperatures(°C)") +
  ggtitle("Annual Mean Temperature for June, July, and August(JJA)")

ggsave("jja_mean_temperature_jmern.png", width = 4, height = 3)
