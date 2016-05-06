library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(gtools)
library(geozoo) # Install from github

raw11 <- read.csv("data/corrected_data_turk11.csv", stringsAsFactors = FALSE)
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
#sim <- rdirichlet(10000, rep(1/19, 19))
#sim <- rdirichlet(10000, rep(1, 19))
#sim <- rdirichlet(10000, rep(1/2, 19))
sim <- rdirichlet(10000, rep(1/3, 19))
#sim <- rdirichlet(10000, rep(0.1, 19))
#sim <- rdirichlet(10000, rep(1/4, 19))
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
  xlab("Order") + ylab("Proportion") +
  scale_colour_hue("Lineup")

# Run the autism example
autism <- raw11[grep("autism", as.character(raw11$param_value)),]

response <- strsplit(autism$response_no, split=",")
autism$wgt <- response %>% purrr::map(.f = length) %>% unlist()
bigautism <- autism %>% purrr::map(.f = function(x) rep(x, autism$wgt)) %>% data.frame()
bigautism$response_no <- unlist(response)
bigautism$wgt <- 1/bigautism$wgt

# compute proportions for each lineup and all responses

filler <- data.frame(expand.grid(
  param_value=unique(bigautism$param_value),
  response_no=1:20))
bigautism <- merge(bigautism, filler, by=c("param_value", "response_no"), all=TRUE)
bigautism$wgt[is.na(bigautism$wgt)] <- 0
bigautism <- bigautism %>% group_by(param_value) %>%
  mutate(plot_location=na.omit(plot_location)[1])
bigautism$detected <- with(bigautism, !(response_no!=plot_location))


nevals <- bigautism %>% filter(detected!=TRUE) %>%
  group_by(param_value) %>% summarise(n=sum(wgt, na.rm=T))

bigautismn <- merge(bigautism, nevals)

resp_prop <- bigautismn %>% filter(detected!=TRUE) %>%
  mutate(pwgt=wgt/n) %>%
  group_by(param_value, response_no) %>%
  summarise(p=sum(pwgt, na.rm=T))

resp_prop_splx <- resp_prop %>% spread(response_no, p)

# Sort from highest to lowest bc order doesn't matter
nsets <- nrow(resp_prop_splx)
for (i in 1:nsets)
  resp_prop_splx[i,-1] <- sort(resp_prop_splx[i,-1], decreasing = TRUE, na.last = TRUE)

# Plot it
resp_prop_splx_long <- resp_prop_splx %>% gather(response_no, p, -param_value)
resp_prop_splx_long$response_no <- rep(1:20,rep(nsets,20))

ggplot(resp_prop_splx_long, aes(x=response_no, y=p, group=param_value, colour=param_value)) +
  geom_line() + scale_colour_hue("Lineup")

# Simulate
sim <- rdirichlet(10000, rep(1/19, 19))
#sim <- rdirichlet(10000, rep(1, 19))
#sim <- rdirichlet(10000, rep(1/2, 19))
sim <- rdirichlet(10000, rep(1/3, 19))
sim <- rdirichlet(10000, rep(0.1, 19))
sim <- rdirichlet(10000, rep(1/4, 19))
sim <- rdirichlet(10000, rep(0.01, 19))
sim <- rdirichlet(10000, rep(0.05, 19))
sim <- rdirichlet(10000, rep(0.06, 19))
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
  xlab("Order") + ylab("Proportion") +
  scale_colour_hue("Lineup")

# Run the cyclone example
cyclone <- raw11[grep("cyclone", as.character(raw11$param_value)),]

response <- strsplit(cyclone$response_no, split=",")
cyclone$wgt <- response %>% purrr::map(.f = length) %>% unlist()
bigcyclone <- cyclone %>% purrr::map(.f = function(x) rep(x, cyclone$wgt)) %>% data.frame()
bigcyclone$response_no <- unlist(response)
bigcyclone$wgt <- 1/bigcyclone$wgt

# compute proportions for each lineup and all responses
filler <- data.frame(expand.grid(
  param_value=unique(bigcyclone$param_value),
  response_no=1:20))
bigcyclone <- merge(bigcyclone, filler, by=c("param_value", "response_no"), all=TRUE)
bigcyclone$wgt[is.na(bigcyclone$wgt)] <- 0
bigcyclone <- bigcyclone %>% group_by(param_value) %>%
  mutate(plot_location=na.omit(plot_location)[1])
bigcyclone$detected <- with(bigcyclone, !(response_no!=plot_location))


nevals <- bigcyclone %>% filter(detected!=TRUE) %>%
  group_by(param_value) %>% summarise(n=sum(wgt, na.rm=T))

bigcyclonen <- merge(bigcyclone, nevals)

resp_prop <- bigcyclonen %>% filter(detected!=TRUE) %>%
  mutate(pwgt=wgt/n) %>%
  group_by(param_value, response_no) %>%
  summarise(p=sum(pwgt, na.rm=T))

resp_prop_splx <- resp_prop %>% spread(response_no, p)

# Sort from highest to lowest bc order doesn't matter
nsets <- nrow(resp_prop_splx)
for (i in 1:nsets)
  resp_prop_splx[i,-1] <- sort(resp_prop_splx[i,-1], decreasing = TRUE, na.last = TRUE)

# Plot it
nchoice <- length(unique(resp_prop_splx_long$response_no))
resp_prop_splx_long <- resp_prop_splx %>% gather(response_no, p, -param_value)
resp_prop_splx_long$response_no <- rep(1:nchoice, rep(nsets,nchoice))

ggplot(resp_prop_splx_long, aes(x=response_no, y=p, group=param_value, colour=param_value)) +
  geom_line() + scale_colour_hue("Lineup")

# Simulate
sim <- rdirichlet(10000, rep(1, 19))
sim <- rdirichlet(10000, rep(2, 19))
sim <- rdirichlet(10000, rep(0.5, 19))
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
  xlab("Order") + ylab("Proportion") +
  scale_colour_hue("Lineup")

# Run the dialyzerheterogeneous example
dialyzerheterogeneous <- raw11[grep("dialyzerheterogeneous", as.character(raw11$param_value)),]

response <- strsplit(dialyzerheterogeneous$response_no, split=",")
dialyzerheterogeneous$wgt <- response %>% purrr::map(.f = length) %>% unlist()
bigdialyzerheterogeneous <- dialyzerheterogeneous %>% purrr::map(.f = function(x) rep(x, dialyzerheterogeneous$wgt)) %>% data.frame()
bigdialyzerheterogeneous$response_no <- unlist(response)
bigdialyzerheterogeneous$wgt <- 1/bigdialyzerheterogeneous$wgt

# compute proportions for each lineup and all responses
filler <- data.frame(expand.grid(
  param_value=unique(bigdialyzerheterogeneous$param_value),
  response_no=1:20))
bigdialyzerheterogeneous <- merge(bigdialyzerheterogeneous, filler, by=c("param_value", "response_no"), all=TRUE)
bigdialyzerheterogeneous$wgt[is.na(bigdialyzerheterogeneous$wgt)] <- 0
bigdialyzerheterogeneous <- bigdialyzerheterogeneous %>% group_by(param_value) %>%
  mutate(plot_location=na.omit(plot_location)[1])
bigdialyzerheterogeneous$detected <- with(bigdialyzerheterogeneous, !(response_no!=plot_location))


nevals <- bigdialyzerheterogeneous %>% filter(detected!=TRUE) %>%
  group_by(param_value) %>% summarise(n=sum(wgt, na.rm=T))

bigdialyzerheterogeneousn <- merge(bigdialyzerheterogeneous, nevals)

resp_prop <- bigdialyzerheterogeneousn %>% filter(detected!=TRUE) %>%
  mutate(pwgt=wgt/n) %>%
  group_by(param_value, response_no) %>%
  summarise(p=sum(pwgt, na.rm=T))

resp_prop_splx <- resp_prop %>% spread(response_no, p)

# Sort from highest to lowest bc order doesn't matter
nsets <- nrow(resp_prop_splx)
for (i in 1:nsets)
  resp_prop_splx[i,-1] <- sort(resp_prop_splx[i,-1], decreasing = TRUE, na.last = TRUE)

# Plot it
nchoice <- length(unique(resp_prop_splx_long$response_no))
resp_prop_splx_long <- resp_prop_splx %>% gather(response_no, p, -param_value)
resp_prop_splx_long$response_no <- rep(1:nchoice, rep(nsets, nchoice))

ggplot(resp_prop_splx_long, aes(x=response_no, y=p, group=param_value, colour=param_value)) +
  geom_line() + scale_colour_hue("Lineup")

# Simulate
sim <- rdirichlet(10000, rep(1, 19))
sim <- rdirichlet(10000, rep(2, 19))
sim <- rdirichlet(10000, rep(0.5, 19))
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
  xlab("Order") + ylab("Proportion") +
  scale_colour_hue("Lineup")

# All lineups
all <- raw11

response <- strsplit(all$response_no, split=",")
all$wgt <- response %>% purrr::map(.f = length) %>% unlist()
bigall <- all %>% purrr::map(.f = function(x) rep(x, all$wgt)) %>% data.frame()
bigall$response_no <- unlist(response)
bigall$wgt <- 1/bigall$wgt

# compute proportions for each lineup and all responses

filler <- data.frame(expand.grid(
  param_value=unique(bigall$param_value),
  response_no=1:20))
bigall <- merge(bigall, filler, by=c("param_value", "response_no"), all=TRUE)
bigall$wgt[is.na(bigall$wgt)] <- 0
bigall <- bigall %>% group_by(param_value) %>%
  mutate(plot_location=na.omit(plot_location)[1])
bigall$detected <- with(bigall, !(response_no!=plot_location))


nevals <- bigall %>% filter(detected!=TRUE) %>%
  group_by(param_value) %>% summarise(n=sum(wgt, na.rm=T))

bigalln <- merge(bigall, nevals)

resp_prop <- bigalln %>% filter(detected!=TRUE) %>%
  mutate(pwgt=wgt/n) %>%
  group_by(param_value, response_no) %>%
  summarise(p=sum(pwgt, na.rm=T))

resp_prop_splx <- resp_prop %>% spread(response_no, p)

# Sort from highest to lowest bc order doesn't matter
nsets <- nrow(resp_prop_splx)
for (i in 1:nsets)
  resp_prop_splx[i,-1] <- sort(resp_prop_splx[i,-1], decreasing = TRUE, na.last = TRUE)

# Plot it
resp_prop_splx_long <- resp_prop_splx %>% gather(response_no, p, -param_value)
nchoice <- length(unique(resp_prop_splx_long$response_no))
resp_prop_splx_long$response_no <- rep(1:nchoice,rep(nsets,nchoice))

ggplot(resp_prop_splx_long, aes(x=response_no, y=p, group=param_value)) +
  geom_line()

# Make simplex
resp_prop_splx <- resp_prop_splx %>% separate(param_value, c("type", "rep"), sep="-",
                                              remove=FALSE)
colnames(resp_prop_splx)[4:23] <- paste("X", 1:20, sep="")
write.csv(resp_prop_splx[,-23], file="data/turk11-proportions.csv")

# Simplex data
s <- simplex(18)
dim(s$points)
dim(s$edges)
write.xml(s, file="data/simplex.xml")

# Combine both data sets
resp_prop_splx$param_value <- as.character(resp_prop_splx$param_value)
all_simplex <- f_composition(resp_prop_splx[,4:22])
colnames(all_simplex) <- paste("X", 1:18, sep="")
all_simplex <- data.frame(resp_prop_splx[,1:3], all_simplex)
colnames(s$points) <- paste("X", 1:18, sep="")
all.s <- data.frame(param_value=NA, type="simplex", rep=NA, s$points)
all_simplex <- rbind(all.s, all_simplex)
all_simplex_xml <- s
all_simplex_xml$points <- all_simplex
write.xml(all_simplex_xml, file="data/turk11_simplex.xml")


# This gives profile plots
resp_prop_splx_long <- resp_prop_splx_long %>% separate(param_value, c("type", "rep"), sep="-",
                                                        remove=FALSE)
write.csv(resp_prop_splx_long, file="data/turk11-proportions_long.csv")
