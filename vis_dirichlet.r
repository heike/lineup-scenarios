library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gtools)

raw11 <- read.csv("corrected_data_turk11.csv", stringsAsFactors = FALSE)
elect <- raw11[grep("elect", as.character(raw11$param_value)),]

response <- strsplit(elect$response_no, split=",")
elect$wgt <- response %>% purrr::map(.f = length) %>% unlist()
bigelect <- elect %>% purrr::map(.f = function(x) rep(x, elect$wgt)) %>% data.frame()
bigelect$response_no <- unlist(response)
bigelect$wgt <- 1/bigelect$wgt

# compute proportions for each lineup and all responses

filler <- data.frame(expand.grid(
  param_value=unique(bigelect$param_value), 
  response_no=1:20))
bigelect <- merge(bigelect, filler, by=c("param_value", "response_no"), all=TRUE)
bigelect$wgt[is.na(bigelect$wgt)] <- 0
bigelect <- bigelect %>% group_by(param_value) %>% 
  mutate(plot_location=na.omit(plot_location)[1])
bigelect$detected <- with(bigelect, !(response_no!=plot_location))


nevals <- bigelect %>% filter(detected!=TRUE) %>% 
  group_by(param_value) %>% summarise(n=sum(wgt, na.rm=T))

bigelectn <- merge(bigelect, nevals)
  
resp_prop <- bigelectn %>% filter(detected!=TRUE) %>% 
  mutate(pwgt=wgt/n) %>%
  group_by(param_value, response_no) %>% 
  summarise(p=sum(pwgt, na.rm=T))

resp_prop_splx <- resp_prop %>% spread(response_no, p)

# Sort from highest to lowest bc order doesn't matter
for (i in 1:5)
  resp_prop_splx[i,-1] <- sort(resp_prop_splx[i,-1], decreasing = TRUE, na.last = TRUE)

# Plot it
resp_prop_splx_long <- resp_prop_splx %>% gather(response_no, p, -param_value)
resp_prop_splx_long$response_no <- rep(1:20,rep(5,20))

ggplot(resp_prop_splx_long, aes(x=response_no, y=p, group=param_value, colour=param_value)) + 
  geom_line() + scale_colour_hue("Lineup")

# Simulate
sim <- rdirichlet(10000, rep(1/19, 19))
sim <- rdirichlet(10000, rep(1, 19))
sim <- rdirichlet(10000, rep(1/2, 19))
sim <- rdirichlet(10000, rep(1/3, 19))
sim <- rdirichlet(10000, rep(0.1, 19))
sim <- rdirichlet(10000, rep(1/4, 19))
for (i in 1:10000)
  sim[i,] <- sort(sim[i,], decreasing = TRUE)

sim_df <- data.frame(sim)
sim_df$id <- 1:10000
sim_df_long <- sim_df %>% gather(response_no, p, -id)
sim_df_long$response_no <- as.numeric(gsub("X", "", sim_df_long$response_no))
#ggplot(sim_df_long, aes(x=response_no, y=p, group=id)) + 
#  geom_line(alpha=0.1)

ggplot(sim_df_long, aes(x=response_no, y=p, group=id)) + 
  geom_line(alpha=0.1) + 
  geom_line(data=resp_prop_splx_long, 
            aes(x=response_no, y=p, group=param_value, colour=param_value)) +  
  scale_colour_hue("Lineup")
