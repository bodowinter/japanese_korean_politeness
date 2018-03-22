## Bodo Winter
## Sep 21, 2017
## Analysis of Japanese perception data

##------------------------------------------------------------------
## Data carpentry:
##------------------------------------------------------------------

## Packages:

library(lme4)
library(afex)
library(stringr)
library(tidyverse)
library(here)

## Load in data:

setwd(here('data'))
jper <- read_csv('JPNpolitenessPerception.csv')

## Rename main dependent variable:

jper <- mutate(jper,
	Resp = `Decision.RESP[Trial]`)

## Create a global trial ID:
## (easier to deal with than with all the different trial IDs)

jper$GlobalTrial <- 1:160

## Get rid of those that have NAs for response:

jper_backup <- jper
jper <- filter(jper,
	!is.na(Resp))
1 - (nrow(jper) / nrow(jper_backup))	# 10.4% exclusion

## Loop through and get which sound file it is:

jper$ChosenFile <- NA
for (i in 1:nrow(jper)) {
	jper[i, ]$ChosenFile <- unlist(select(jper[i, ], SoundFile, SoundFile2))[jper[i, ]$Resp]
	}

## Get rid of those that have NAs for response:

jper_backup <- jper
jper <- filter(jper,
	!is.na(Resp))
1 - (nrow(jper) / nrow(jper_backup))	# 10.4% exclusion

## Rename chosen file:

jper$PoliteResponse <- str_split(jper$ChosenFile,
	pattern = '(_|\\.)',
	simplify = TRUE)[, 3]
jper <- mutate(jper,
	PoliteResponse = ifelse(PoliteResponse == 'cas', 'casual', 'polite'))

## Recode '`Running[Trial]`':

jper <- rename(jper,
	PairRepetition = `Running[Trial]`) %>%
	mutate(PairRepetition = ifelse(PairRepetition == 'SenarioList', 1, 2))

## Rename RT variable and log-transform:

jper <- rename(jper,
	RT = `Decision.RT[Trial]`) %>%
	mutate(LogRT = log10(RT + 1))

## Center continuous variables:

jper <- mutate(jper,
	LogRT_c = LogRT - mean(LogRT),
	GlobalTrial_c = GlobalTrial - mean(GlobalTrial),
	Trial_c = Trial - mean(Trial))

## Create accuracy measure:

jper$ACC <- NA
jper[jper$PoliteResponse == 'casual', ]$ACC <- 0
jper[jper$PoliteResponse == 'polite', ]$ACC <- 1

## Create accuracy factor measure for mixed model:

jper <- mutate(jper,
	ACC_fac = factor(ACC),
	Resp_fac = factor(Resp))

## Create items from file name:

jper <- mutate(jper,
	item = str_extract(SoundFile, '_[0-9]+_'),
	item = str_replace_all(item, '_', ''))

## Get speaker gender:

jper_speaker <- read_csv('JPNpoliteness_PerceptionStimuli_AcousticData.csv')
jper_speaker <- filter(jper_speaker, !duplicated(speaker)) %>%
	select(speaker, gender) %>%
	rename(SpeakerNumber = speaker, SpeakerSex = gender)
jper <- left_join(jper, jper_speaker)

## Get average accuracy:

mean(jper$ACC)



##------------------------------------------------------------------
## Logistic mixed regression analysis:
##------------------------------------------------------------------

## Effect code sex predictors:

jper <- mutate(jper,
	SpeakerSex = factor(SpeakerSex),
	Sex = factor(Sex))
contrasts(jper$SpeakerSex) <- contr.sum(2) / 2
contrasts(jper$Sex) <- contr.sum(2) / 2

## Center pair repetition:

jper <- mutate(jper,
	PairRepetition_c = PairRepetition - 1.5)

## Create main model:

print(ACC_mdl <- mixed(ACC_fac ~ SpeakerSex * Sex + Trial_c + PairRepetition_c + 
	(1|Subject) + (1|item) + (1|SpeakerNumber),
	data = jper, family = 'binomial', method = 'LRT'))

## Look at intercept of main model:

summary(ACC_mdl$full.model)

## Check accuracy by gender:

aggregate(ACC ~ SpeakerSex, jper, mean)



##------------------------------------------------------------------
## Comparison to Korean:
##------------------------------------------------------------------

## Load in Korean perceptual data:

setwd(here('data'))
kper <- read_csv('KRNpolitenessPerception_2014bdata.csv')

## Rename main dependent variable:

kper <- mutate(kper,
	Resp = Decision.RESP)

## Create a global trial ID:
## (easier to deal with than with all the different trial IDs)

kper$GlobalTrial <- 1:160

## Get rid of those that have NAs for response:

kper_backup <- kper
kper <- filter(kper,
	!is.na(Resp))
1 - (nrow(kper) / nrow(kper_backup))	# 0% exclusion

## Loop through and get which sound file it is:

kper$ChosenFile <- NA
for (i in 1:nrow(jper)) {
	kper[i, ]$ChosenFile <- unlist(select(kper[i, ], SoundFile, SoundFile2))[kper[i, ]$Resp]
	}

## Get rid of those that have NAs for response:

kper_backup <- kper
kper <- filter(kper,
	!is.na(Resp))
1 - (nrow(kper) / nrow(kper_backup))	# 0.46% exclusion

## Rename chosen file:

kper$PoliteResponse <- str_split(kper$ChosenFile,
	pattern = '(_|\\.)',
	simplify = TRUE)[, 3]
kper <- mutate(kper,
	PoliteResponse = ifelse(PoliteResponse == 'pan', 'casual', 'polite'))

## Recode '`Running[Trial]`':

kper <- rename(kper,
	PairRepetition = `Running[Trial]`) %>%
	mutate(PairRepetition = ifelse(PairRepetition == 'SenarioList', 1, 2))

## Rename RT variable and log-transform:

kper <- rename(kper,
	RT = Decision.RT) %>%
	mutate(LogRT = log10(RT + 1))

## Center continuous variables:

kper <- mutate(kper,
	LogRT_c = LogRT - mean(LogRT),
	GlobalTrial_c = GlobalTrial - mean(GlobalTrial),
	Trial_c = Trial - mean(Trial))

## Create accuracy measure:

kper$ACC <- NA
kper[kper$PoliteResponse == 'casual', ]$ACC <- 0
kper[kper$PoliteResponse == 'polite', ]$ACC <- 1

## Create accuracy factor measure for mixed model:

kper <- mutate(kper,
	ACC_fac = factor(ACC),
	Resp_fac = factor(Resp))

## Create items from file name:

kper <- mutate(kper,
	item = str_extract(SoundFile, '_[0-9]+_'),
	item = str_replace_all(item, '_', ''))

## Get speaker gender:

kper_speaker <- read_csv('JPNpoliteness_PerceptionStimuli_AcousticData.csv')
kper_speaker <- filter(kper_speaker, !duplicated(speaker)) %>%
	select(speaker, gender) %>%
	rename(SpeakerNumber = speaker, SpeakerSex = gender)
kper <- left_join(kper, kper_speaker)

## Get average accuracy:

mean(kper$ACC)

## Effect code sex predictors:

kper <- mutate(kper,
	SpeakerSex = factor(SpeakerSex),
	Sex = factor(Sex))
contrasts(kper$SpeakerSex) <- contr.sum(2) / 2
contrasts(kper$Sex) <- contr.sum(2) / 2

## Center pair repetition:

kper <- mutate(kper,
	PairRepetition_c = PairRepetition - 1.5)

## Select subsets that match:

kper_red <- select(kper, ACC, Subject, item, SpeakerNumber) %>%
	rename(speaker = SpeakerNumber)
jper_red <- select(jper, ACC, Subject, item, speaker)

## Merge the two files:

both <- bind_rows(kper_red, jper_red)
both$Language <- c(rep('Korean', nrow(kper_red)),
	rep('Japanese', nrow(jper_red)))

## Create unique listener, speaker and item identifier:

both <- mutate(both,
	uniqueSub = str_c(Language, '_', Subject),
	uniqueIt = str_c(Language, '_', item),
	uniqueSpeaker = str_c(Language, '_', speaker))

## Create main model:

print(ACC_both <- mixed(ACC ~ Language + 
	(1|uniqueSub) + (1|uniqueIt) + (1|uniqueSpeaker),
	data = both, family = 'binomial', method = 'LRT'))
summary(ACC_both$full.model)

## Merge the two files:

kper_red <- select(kper, ACC, 





##------------------------------------------------------------------
## Random forests:
##------------------------------------------------------------------

library(ranger)

## Load in z-scored processed data from production analysis:

jprod_z <- read_csv('japanese_production_z-scored.csv')

## Impute missing range values:

jprod_z[is.na(jprod_z$rate), ]$rate <- mean(jprod_z$rate,
	na.rm = TRUE)
jprod_z <- mutate(jprod_z,
	condition2 = as.factor(condition2))

## Get clean item identifiers out of the jprod data frame:

jprod_z <- mutate(jprod_z,
	item = str_extract(item, '([0-9])+'))

## There are different numbers of data points per participants:
## So I need to loop through things to calculate difference scores:

jprod_z <- mutate(jprod_z,
	unique_ID = str_c(task, scenario))
all_prod_speakers <- sort(unique(jprod_z$speaker))
all_items <- sort(unique(jprod_z$unique_ID))

## Create results data frame:

all_res <- rep(NA, length(all_prod_speakers) * length(all_items) * 11)
all_res <- matrix(all_res, nrow = length(all_prod_speakers) * length(all_items))
all_res[, 1] <- rep(all_prod_speakers, each = length(all_items))
all_res[, 2] <- rep(all_items, times = length(all_prod_speakers))
one_res <- all_res[all_res[, 1] == "1", ][, 3:11]

## Loop!

for (i in seq_along(all_prod_speakers)) {
	this_s <- all_prod_speakers[i]
	this_df <- filter(jprod_z, speaker == this_s)
	
	this_res_df <- one_res
	for (j in seq_along(all_items)) {
		this_i <- all_items[j]
		this_item <- filter(this_df, unique_ID == this_i)
		this_item <- arrange(this_item, condition2)
	
		if (nrow(this_item) == 2) {
			myM <- as.matrix(select(this_item, dur, rate, inmd, f0mnhz, f0sdhz, jitloc,
				shimloc, h1mh2mn, mnHNR))
			myM <- myM[2, ] - myM[1, ]	# polite - informal
			this_res_df[j, ] <- myM
			}	# else { this_res_df[j, ] <- NA }
		}
	
	all_res[all_res[, 1] == as.character(this_s), 3:11] <- this_res_df
	}

## Format results data frame:

all_res <- as_tibble(all_res)
mycols <- c('dur', 'rate', 'inmd', 'f0mnhz', 'f0sdhz',
	'jitloc', 'shimloc', 'h1mh2mn', 'mnHNR')
colnames(all_res) <- c('speaker', 'unique_ID', mycols)
all_res[, mycols] <- apply(all_res[, mycols], 2, as.numeric)

## Rename columns in jper data frame for comparibility:

jper <- rename(jper,
	speaker = SpeakerNumber,
	condition2 = PoliteResponse)

## Get rid of those speakers that are not in jper:

all_res <- filter(all_res, speaker %in% jper$speaker)

## Z-score within speaker:

all_speakers <- unique(jper$speaker)
for (i in seq_along(all_speakers)) {
	this_s <- all_speakers[i]
	this_df <- filter(all_res, speaker == this_s)
	all_res[all_res$speaker == this_s, mycols] <- apply(this_df[, mycols],
		MARGIN = 2, FUN = function(x) unlist(scale(x)))
	}

## Merge perception and production:

all_res <- filter(all_res,
	str_detect(unique_ID, 'epr'))
all_res <- mutate(all_res,
	item = str_extract(unique_ID, '([0-9])+'),
	speaker = as.integer(speaker),
	speaker_item = str_c(speaker, '_', item))
jper <- mutate(jper,
	speaker_item = str_c(speaker, '_', item))
jper <- bind_cols(jper, all_res[match(jper$speaker_item, all_res$speaker_item), mycols])

## Make a random forest:
## (to double check that I coded everything correctly, I ran it twice)

set.seed(42)
jap.forest <- ranger(as.factor(ACC) ~ dur + rate + inmd + f0mnhz + f0sdhz +
	jitloc + shimloc + h1mh2mn + mnHNR, data = jper,
	importance = 'permutation', num.trees = 5000, mtry = 3)
set.seed(42)
jap.forest2 <- ranger(condition2 ~ dur + rate + inmd + f0mnhz + f0sdhz +
	jitloc + shimloc + h1mh2mn + mnHNR, data = jper,
	importance = 'permutation', num.trees = 5000, mtry = 3)

## Look at variable importances:

sort(jap.forest$variable.importance)
sort(jap.forest2$variable.importance)

## Get predictions to double check classification accuracy:

jap.forest_preds <- predict(jap.forest, data = jper)
mypreds <- table(jper$ACC, jap.forest_preds$predictions)
sum(diag(mypreds)) / sum(mypreds)	# 64%

## Split dataset into test and training set:

set.seed(42)
train_ids <- sample(1:nrow(jper), 2005)
test_ids <- (1:nrow(jper))[!(1:nrow(jper) %in% train_ids)]
jper_train <- jper[train_ids, ]
jper_test <- jper[test_ids, ]

## Train random forest 

set.seed(42)
jap.forest_train <- ranger(as.factor(ACC) ~ dur + rate + inmd + f0mnhz + f0sdhz +
	jitloc + shimloc + h1mh2mn + mnHNR, data = jper_train,
	importance = 'permutation', num.trees = 5000, mtry = 3)

## Get predictions for test data:

jap_forest_crossv <- predict(jap.forest_train, data = jper_test)
mypreds_test <- table(jper_test$ACC, jap_forest_crossv$predictions)
sum(diag(mypreds_test)) / sum(mypreds_test)	# 60.4

## Compare variable importances:

sort(jap.forest_train$variable.importance)
sort(jap.forest$variable.importance)	# duration consistently on top

## Make a plot of this:

myvars <- sort(jap.forest$variable.importance)
myvars_names <- c('HNR', 'Pitch SD', 'Pitch', 'Intensity',
	'Shimmer', 'Jitter', 'Rate', 'H1-H2', 'Duration')

## Plot of variable importances:

quartz('', 9, 6)
par(mai = c(1.5, 1.5, 0.5, 0.5))
plot(1, 1, type = 'n', xlab = '', ylab = '', xaxt = 'n', yaxt = 'n', bty = 'n',
	xlim = c(0, 0.03), ylim = c(0, 10))
mtext(side = 1, text = 'Relative Variable Importance', line = 4, font = 2, cex = 2)
mtext(side = 1, text = '(permutation based)', line = 6, font = 2, cex = 1.5)
abline(h = 1:9, lty = 2, col = 'darkgrey')
axis(side = 1, at = seq(0, 0.03, 0.01), lwd = 2, cex.axis = 1.5, font = 2)
axis(side = 2, at = 1:9, labels = myvars_names,
	lwd = 2, cex.axis = 1.5, font = 2, las = 2)
points(x = myvars, y = 1:9, pch = 15, cex = 1.5)
box(lwd = 2)
abline(v = 0)


