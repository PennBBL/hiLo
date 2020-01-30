### This script is some ggseg brainstorming
###
### Ellyn Butler
### January 8, 2020

library('ggseg')
library('ggplot2')
library('tidyverse')

base_atlas = dkt %>%
  select(area,hemi) %>%
  distinct() %>%
  na.omit()

new_data = base_atlas %>%
  mutate(Modality="Volume", EffectSize=sample(seq(.6,.7,by=.001), nrow(.)))

one_mod <- ggseg(new_data, mapping=aes(fill=EffectSize), position="stacked", colour="darkgrey", size=.1) +
  scale_fill_gradientn(colours=c("dodgerblue4","lightblue","white","goldenrod","firebrick"))

# Make a list of all the age groups
modalities = c("Volume", "GMD", "MD", "CBF", "ALFF", "ReHo", "NBack", "IdEmo")
full_data = sapply(modalities, list)
names(full_data) = modalities

# apply through the list to fabricate data per age.
full_data = lapply(full_data,
    function(x)
      base_atlas %>%
      mutate(EffectSize = sample(seq(.6,.7,by=.001), nrow(.))) %>%
          full_join(dkt) %>%
          mutate(Modality=x[1])
          )

# Bind the data together.
full_data = full_data %>% bind_rows()

full_data <- full_data[full_data$hemi == "right",]

full_data_gender <- rbind(full_data, full_data)
full_data_gender$hemi <- NULL
full_data_gender$Gender <- c(rep("Female", 320), rep("Male", 320))
full_data_gender$Modality <- ordered(full_data_gender$Modality, levels=modalities)

all_mods <- ggseg(full_data_gender, mapping=aes(fill=EffectSize), colour="darkgrey", hemisphere="right", size=.1) +
  scale_fill_gradientn(colours=c("dodgerblue4","lightblue","white","goldenrod","firebrick")) +
  facet_grid(Modality~Gender)


# TO DO:
# 1. Get left out of plot
# 2. Get hemisphere info out of plot
# 3.


#### Organized plot

p1_F <- ggseg(position="stacked") + ggtitle("") + theme(plot.title = element_text(size=30))
p1_M <- ggseg(position="stacked") + ggtitle("") + theme(plot.title = element_text(size=30))
p2_F <- ggseg(atlas="aseg") + ggtitle("Female") + theme(plot.title = element_text(size=30))
p2_M <- ggseg(atlas="aseg") + ggtitle("Male") + theme(plot.title = element_text(size=30))

pdf(file="/Users/butellyn/Documents/hiLo/plots/arranged_trial.pdf", width=10, height=8)
grid.arrange(p2_F, p2_M, p1_F, p1_M, ncol=2, nrow=2)
dev.off()
