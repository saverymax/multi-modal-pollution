# Script to generate covid ts for each Belgian region
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readxl)

# Next get traffic and COVID data.
user_dir = "D:/asus_documents/ku_leuven/thesis/code/multi-modal-pollution"
setwd(user_dir)
covid_df <- read_excel("data/covid/covid19be.xlsx")
names(covid_df) <- tolower(names(covid_df))
head(covid_df)
# Lockdown times for brussels
lockdown_regions <- data.frame(xmin=c(as.POSIXlt("2020-03-18"), as.POSIXlt("2020-10-28")), 
                               xmax=c(as.POSIXlt("2020-05-06"), as.POSIXlt("2021-05-08")), 
                               ymin=-Inf, ymax=Inf)
regions <- c("Flanders", "Brussels", "Wallonia")
for (r_i in 1:length(regions)){
    reg <- regions[r_i]
    print(reg)
    png(filename=paste(user_dir, "/data/figures/covid_ts/", reg, "_ts.png", sep=""), width=700)
    covid_df %>% dplyr::filter(region==reg) %>% 
    mutate(day=lubridate::ceiling_date(date, "day")) %>% 
    group_by(day) %>% summarise(total_cases=sum(cases)) %>% 
    dplyr::filter(day > "2020-03-01" & day <= "2021-06-02") %>% 
    ggplot(aes(x=day, y=total_cases)) +
        geom_line() + 
        ggtitle(reg) +
        xlab("Date") +
        ylab("Daily cases") + theme_bw(base_size=25) + 
        theme(title = element_text(size=25), 
              axis.text.y = element_text(size = 22),
              axis.text.x = element_text(size = 22, angle=45, vjust = .6)) +
        geom_rect(data=lockdown_regions, alpha=.3, inherit.aes=FALSE, 
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="steelblue3") -> p
    print(p)
    dev.off()
}
