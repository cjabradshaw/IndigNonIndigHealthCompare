## Indigenous vs. non-Indigenous health comparisons
## October 2026
## Corey Bradshaw

## libraries
library(ggplot2)
library(dplyr)
library(countrycode)
library(ggpubr)

## import data
dat <- read.csv("data/I_NI_outcomeOR3.csv")
head(dat)
str(dat)

## bootstrapped OR by outcome
outcomes_vec <- names(table(dat$outcome))
outcomes_vec
boot_iter <- 1000

outcome_OR_med <- outcome_OR_lo <- outcome_OR_up <- rep(NA, length(outcomes_vec))
for (o in 1:length(outcomes_vec)) {
  outcome_dat <- subset(dat, outcome == outcomes_vec[o])
  
  OR_boot_vec <- NA
  ## iterate fewer times for random study selection
  for (s in 1:round(boot_iter/10, 0)) {
    
    ## only take 1 value from the same study (at random) per outcome iteration
    stud_unique <- unique(outcome_dat$STUD)
    stud_dat_unique <- outcome_dat[1,]
    for (s in 1:length(stud_unique)) {
      stud_dat <- subset(outcome_dat, STUD == stud_unique[s])
      lstud_dat <- dim(stud_dat)[1]
      if (lstud_dat > 1) {
        entry_singl_stud <- stud_dat[sample(1:lstud_dat)[1],]
      }
      if (lstud_dat == 1) {
        entry_singl_stud <- stud_dat
      }
      stud_dat_unique <- rbind(stud_dat_unique, entry_singl_stud)
    }
    stud_dat_unique <- stud_dat_unique[-1,]
    
    for (i in 1:dim(stud_dat_unique)[1]) {
      mu <- log(stud_dat_unique[i,]$OR) # log odds
      SE <- stud_dat_unique[i,]$ORSE # log odds SE
      OR_boot <- exp(rnorm(boot_iter, mean = mu, sd = SE)) # Gaussian resample and back-transform
      OR_boot_vec <- c(OR_boot_vec, OR_boot)
    } # end i loop
    
  } # end s loop
  OR_boot_vec <- OR_boot_vec[-1]
  
  outcome_OR_med[o] <- median(OR_boot_vec, na.rm=T)
  outcome_OR_lo[o] <- quantile(OR_boot_vec, probs=0.025, na.rm=T)
  outcome_OR_up[o] <- quantile(OR_boot_vec, probs=0.975, na.rm=T)
  print(paste("outcome = ", outcomes_vec[o], "; % complete = ",
              round(100*o/length(outcomes_vec), 1), "%", sep=""))
  
} # end o loop

outcome_dat_boot <- data.frame("outcome" = outcomes_vec,
                               "ORmed" = outcome_OR_med,
                               "ORup" = outcome_OR_up,
                               "ORlo" = outcome_OR_lo)
outcome_dat_boot
outcome_dat_boot_sort <- outcome_dat_boot[order(outcome_dat_boot$ORmed, decreasing=TRUE),]
outcome_dat_boot_sort

## ggplot barplot of outcome_dat_boot_sort with error bars by outcome
ggplot(
  outcome_dat_boot_sort,
  aes(x = reorder(outcome, ORmed), y = ORmed)
) +
  scale_y_log10() +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = ORlo, ymax = ORup), width = 0.3, linewidth = 0.3, col="grey40") +
  coord_flip() +
  labs(x = "outcome", y = "median odds ratio") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))



## tables of studies & entries X country
head(dat)
summXcntry <- dat %>%
  group_by(cntry.code) %>%
  summarise(
    n_entries = n(),
    stud_uniq = length(unique(STUD))
  )
summXcntry

## output for QGIS joins for plotting
cont_cntry <- read.csv("data/cont_cntry.csv", stringsAsFactors = FALSE)
iso_lookup <- codelist |>
  dplyr::select(country.name.en, iso2c, iso3c) |>
  dplyr::distinct() |>
  dplyr::arrange(iso3c)
cont_cntry2 <- merge(
  cont_cntry,
  iso_lookup,
  by.x = "cntry.code",
  by.y = "iso3c",
  all.x = TRUE
)
head(cont_cntry2)

## merge with summXcntry
summXcntry_iso2 <- merge(summXcntry, cont_cntry2, by="cntry.code")
head(summXcntry_iso2)

write.table(summXcntry_iso2, file="summXcntry.csv", sep=",", row.names=FALSE)

## tables of studies & entries X outcome
head(dat)
summXoutcome <- dat %>%
  group_by(outcome) %>%
  summarise(
    n_entries = n(),
    stud_uniq = length(unique(STUD))
  )
summXoutcome
summXoutcome_sort_entry <- summXoutcome[order(summXoutcome$n_entries, decreasing=TRUE), ]
summXoutcome_sort_entry

summXoutcome_sort_study <- summXoutcome[order(summXoutcome$stud_uniq, decreasing=TRUE), ]
summXoutcome_sort_study

entriesXoutcome_plot <- ggplot(
  summXoutcome_sort_entry,
  aes(x = reorder(outcome, n_entries), y = n_entries)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "outcome", y = "number entries") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
entriesXoutcome_plot

studiesXoutcome_plot <- ggplot(
  summXoutcome_sort_study,
  aes(x = reorder(outcome, n_entries), y = stud_uniq)
) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(x = "", y = "number studies") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
studiesXoutcome_plot

ggarrange(entriesXoutcome_plot, studiesXoutcome_plot, ncol=2, nrow=1)


## time series
# calculate number of studies/entries over time (by year)
head(dat)
studXyear <- dat %>%
  group_by(year) %>%
  summarise(
    n_entries = n(),
    stud_uniq = length(unique(STUD))
  )
studXyear
studXyear$n_entries_cum <- cumsum(studXyear$n_entries)
studXyear$stud_uniq_cum <- cumsum(studXyear$stud_uniq)
studXyear

## plot
breaks_major <- 10^(0:5)
breaks_minor <- unlist(lapply(0:5, function(i) (2:9) * 10^i))

stud_entr_year_plot <- ggplot(
  studXyear, 
  aes(x = year)) +
  geom_line(aes(y = stud_uniq_cum), linewidth=2, col = "black") +
  geom_line(aes(y = n_entries_cum), linewidth=2, col = "steelblue") +
  annotate("text", x = 2000, y = 20, label = "studies", size = 8, colour = "black") +
  annotate("text", x = 2000, y = 100, label = "entries", size = 8, colour = "steelblue") +
  scale_y_log10(breaks = breaks_major,
                minor_breaks = breaks_minor) +
    labs(y = "cumulative number", x = "year") +
  theme(axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))
stud_entr_year_plot

## cycle through years and resample odds ratios to examine for trends through time
years_vec <- as.numeric(names(table(dat$year)))
years_vec
boot_iter <- 1000

year_OR_med <- year_OR_lo <- year_OR_up <- rep(NA, length(years_vec))
for (y in 1:length(years_vec)) {
  year_dat <- subset(dat, year == years_vec[y])
  
  OR_boot_vec <- NA
  ## iterate fewer times for random study selection
  for (s in 1:round(boot_iter/10, 0)) {
    
    ## only take 1 value from the same study (at random) per outcome iteration
    stud_unique <- unique(year_dat$STUD)
    stud_dat_unique <- year_dat[1,]
    for (s in 1:length(stud_unique)) {
      stud_dat <- subset(year_dat, STUD == stud_unique[s])
      lstud_dat <- dim(stud_dat)[1]
      if (lstud_dat > 1) {
        entry_singl_stud <- stud_dat[sample(1:lstud_dat)[1],]
      }
      if (lstud_dat == 1) {
        entry_singl_stud <- stud_dat
      }
      stud_dat_unique <- rbind(stud_dat_unique, entry_singl_stud)
    }
    stud_dat_unique <- stud_dat_unique[-1,]
    
    for (i in 1:dim(stud_dat_unique)[1]) {
      mu <- log(stud_dat_unique[i,]$OR) # log odds
      SE <- stud_dat_unique[i,]$ORSE # log odds SE
      OR_boot <- exp(rnorm(boot_iter, mean = mu, sd = SE)) # Gaussian resample and back-transform
      OR_boot_vec <- c(OR_boot_vec, OR_boot)
    } # end i loop
    
  } # end s loop
  OR_boot_vec <- OR_boot_vec[-1]
  
  year_OR_med[y] <- median(OR_boot_vec, na.rm=T)
  year_OR_lo[y] <- quantile(OR_boot_vec, probs=0.025, na.rm=T)
  year_OR_up[y] <- quantile(OR_boot_vec, probs=0.975, na.rm=T)
  print(paste("year = ", years_vec[y], "; % complete = ",
              round(100*y/length(years_vec), 1), "%", sep=""))
  
} # end y loop

year_dat_boot <- data.frame("year" = years_vec,
                               "ORmed" = year_OR_med,
                               "ORup" = year_OR_up,
                               "ORlo" = year_OR_lo)
year_dat_boot
head(year_dat_boot)

## plot
breaks_major <- 10^(0:5)
breaks_minor <- unlist(lapply(0:5, function(i) (2:9) * 10^i))

ORXyear_plot <- ggplot(
  year_dat_boot, 
  aes(x = year)) +
  geom_line(aes(y = ORmed), linewidth=2, col = "black") +
  geom_point(aes(y = ORmed), size=4, col="darkred") +
  geom_errorbar(
    aes(ymin = ORlo, ymax = ORup),
    width = 0.3, linewidth = 0.3, col="grey40"
  ) +
  scale_y_log10(breaks = breaks_major,
                minor_breaks = breaks_minor) +
  labs(y = "odds ratio", x = "year") +
  annotate("text", x = 1970, y = 400, label = "A", size = 15, colour = "black") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16))
ORXyear_plot


## calculate bootstrapped OR time series for SEVSTU only (enough entries with OR > 1)
SEVSTU.dat <- subset(dat, outcome == "SEVSTU")
head(SEVSTU.dat)
dim(SEVSTU.dat)
range(SEVSTU.dat$year)

SEVSTU.years_vec <- as.numeric(names(table(SEVSTU.dat$year)))
SEVSTU.years_vec
boot_iter <- 1000

SEVSTU.year_OR_med <- SEVSTU.year_OR_lo <- SEVSTU.year_OR_up <- rep(NA, length(SEVSTU.years_vec))
for (y in 1:length(SEVSTU.years_vec)) {
  year_dat <- subset(SEVSTU.dat, year == SEVSTU.years_vec[y])
  
  OR_boot_vec <- NA
  ## iterate fewer times for random study selection
  for (s in 1:round(boot_iter/10, 0)) {
    
    ## only take 1 value from the same study (at random) per outcome iteration
    stud_unique <- unique(year_dat$STUD)
    stud_dat_unique <- year_dat[1,]
    for (s in 1:length(stud_unique)) {
      stud_dat <- subset(year_dat, STUD == stud_unique[s])
      lstud_dat <- dim(stud_dat)[1]
      if (lstud_dat > 1) {
        entry_singl_stud <- stud_dat[sample(1:lstud_dat)[1],]
      }
      if (lstud_dat == 1) {
        entry_singl_stud <- stud_dat
      }
      stud_dat_unique <- rbind(stud_dat_unique, entry_singl_stud)
    }
    stud_dat_unique <- stud_dat_unique[-1,]
    
    for (i in 1:dim(stud_dat_unique)[1]) {
      mu <- log(stud_dat_unique[i,]$OR) # log odds
      SE <- stud_dat_unique[i,]$ORSE # log odds SE
      OR_boot <- exp(rnorm(boot_iter, mean = mu, sd = SE)) # Gaussian resample and back-transform
      OR_boot_vec <- c(OR_boot_vec, OR_boot)
    } # end i loop
    
  } # end s loop
  OR_boot_vec <- OR_boot_vec[-1]
  
  SEVSTU.year_OR_med[y] <- median(OR_boot_vec, na.rm=T)
  SEVSTU.year_OR_lo[y] <- quantile(OR_boot_vec, probs=0.025, na.rm=T)
  SEVSTU.year_OR_up[y] <- quantile(OR_boot_vec, probs=0.975, na.rm=T)
  print(paste("year = ", SEVSTU.years_vec[y], "; % complete = ",
              round(100*y/length(SEVSTU.years_vec), 1), "%", sep=""))
  
} # end y loop

SEVSTU.year_dat_boot <- data.frame("year" = SEVSTU.years_vec,
                            "ORmed" = SEVSTU.year_OR_med,
                            "ORup" = SEVSTU.year_OR_up,
                            "ORlo" = SEVSTU.year_OR_lo)
SEVSTU.year_dat_boot
head(SEVSTU.year_dat_boot)

## plot
breaks_major <- 10^(0:5)
breaks_minor <- unlist(lapply(0:5, function(i) (2:9) * 10^i))

SEVSTU.ORXyear_plot <- ggplot(
  SEVSTU.year_dat_boot, 
  aes(x = year)) +
  geom_line(aes(y = ORmed), linewidth=2, col = "black") +
  geom_point(aes(y = ORmed), size=4, col="darkred") +
  geom_errorbar(
    aes(ymin = ORlo, ymax = ORup),
    width = 0.3, linewidth = 0.3, col="grey40"
  ) +
  scale_y_log10(breaks = breaks_major, minor_breaks = breaks_minor) +
  labs(y = "odds ratio", x = "year") +
  annotate("text", x = 1993, y = 2400, label = "B", size = 15, colour = "black") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank())
SEVSTU.ORXyear_plot


## calculate bootstrapped OR time series for MODSTU only (enough entries with OR > 1)
MODSTU.dat <- subset(dat, outcome == "MODSTU")
head(MODSTU.dat)
dim(MODSTU.dat)
range(MODSTU.dat$year)

MODSTU.years_vec <- as.numeric(names(table(MODSTU.dat$year)))
MODSTU.years_vec
boot_iter <- 1000

MODSTU.year_OR_med <- MODSTU.year_OR_lo <- MODSTU.year_OR_up <- rep(NA, length(MODSTU.years_vec))
for (y in 1:length(MODSTU.years_vec)) {
  year_dat <- subset(MODSTU.dat, year == MODSTU.years_vec[y])
  
  OR_boot_vec <- NA
  ## iterate fewer times for random study selection
  for (s in 1:round(boot_iter/10, 0)) {
    
    ## only take 1 value from the same study (at random) per outcome iteration
    stud_unique <- unique(year_dat$STUD)
    stud_dat_unique <- year_dat[1,]
    for (s in 1:length(stud_unique)) {
      stud_dat <- subset(year_dat, STUD == stud_unique[s])
      lstud_dat <- dim(stud_dat)[1]
      if (lstud_dat > 1) {
        entry_singl_stud <- stud_dat[sample(1:lstud_dat)[1],]
      }
      if (lstud_dat == 1) {
        entry_singl_stud <- stud_dat
      }
      stud_dat_unique <- rbind(stud_dat_unique, entry_singl_stud)
    }
    stud_dat_unique <- stud_dat_unique[-1,]
    
    for (i in 1:dim(stud_dat_unique)[1]) {
      mu <- log(stud_dat_unique[i,]$OR) # log odds
      SE <- stud_dat_unique[i,]$ORSE # log odds SE
      OR_boot <- exp(rnorm(boot_iter, mean = mu, sd = SE)) # Gaussian resample and back-transform
      OR_boot_vec <- c(OR_boot_vec, OR_boot)
    } # end i loop
    
  } # end s loop
  OR_boot_vec <- OR_boot_vec[-1]
  
  MODSTU.year_OR_med[y] <- median(OR_boot_vec, na.rm=T)
  MODSTU.year_OR_lo[y] <- quantile(OR_boot_vec, probs=0.025, na.rm=T)
  MODSTU.year_OR_up[y] <- quantile(OR_boot_vec, probs=0.975, na.rm=T)
  print(paste("year = ", MODSTU.years_vec[y], "; % complete = ",
              round(100*y/length(MODSTU.years_vec), 1), "%", sep=""))
  
} # end y loop

MODSTU.year_dat_boot <- data.frame("year" = MODSTU.years_vec,
                                   "ORmed" = MODSTU.year_OR_med,
                                   "ORup" = MODSTU.year_OR_up,
                                   "ORlo" = MODSTU.year_OR_lo)
MODSTU.year_dat_boot
head(MODSTU.year_dat_boot)

## plot
breaks_major <- 10^(0:5)
breaks_minor <- unlist(lapply(0:5, function(i) (2:9) * 10^i))

MODSTU.ORXyear_plot <- ggplot(
  MODSTU.year_dat_boot, 
  aes(x = year)) +
  geom_line(aes(y = ORmed), linewidth=2, col = "black") +
  geom_point(aes(y = ORmed), size=4, col="darkred") +
  geom_errorbar(
    aes(ymin = ORlo, ymax = ORup),
    width = 0.3, linewidth = 0.3, col="grey40"
  ) +
  scale_y_log10(breaks = breaks_major, minor_breaks = breaks_minor) +
  labs(y = "odds ratio", x = "year") +
  annotate("text", x = 1992, y = 490, label = "C", size = 15, colour = "black") +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_blank())
MODSTU.ORXyear_plot

## plot 3 outcomes together
ggarrange(ORXyear_plot, SEVSTU.ORXyear_plot, MODSTU.ORXyear_plot, ncol=3, nrow=1)
