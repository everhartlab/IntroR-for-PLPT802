#' ---
#' title: Recreating Figure 2 of Miorini et al.
#' output: html_document
#' ---
#'
#' We are going to be recreating Figure 2 from the 2017 Morini, Raetano, and
#' Everhart paper "Control of white mold of dry bean and residual activity of
#' fungicides applied by chemigation." (DOI:
#' [10.1016.j/cropro.2016.12.023](https://dx.doi.org/10.1016/j.cropro.2016.12.023)).
#'
#' The data are stored in a spreadsheet called "Results_combined-class.csv".
#'
#' Preparing Data and packages
#' ===========================
#'
#'
#' We will recreate the figure using dplyr, tidyr, and ggplot2
#+ package_loading, warning = FALSE, message = FALSE
library("dplyr")
library("tidyr")
library("ggplot2")
#'
#' Now we can read in the data and validate that it is what we expect.
res <- read.csv("Results_combined-class.csv", head = TRUE, stringsAsFactors = FALSE)
head(res)
dim(res)
#' We can see that we have the columns, DAI, Number, Your.name, Area, and
#' Treatment.
#'
#' Simulation
#' ----------
#'
#' For the moment, I'm going to simulate some data to recreate the
#' figure, so this block of code can be deleted when the data is all filled.
#'
#' For each treatment, we are going to randomly sample from a normal
#' distribution with a mean from 1 to 10. Then, if the treatment is "control",
#' we'll multiply that area by 3 to make sure the effect is large, otherwise
#' each value will grow by 0.5 each day after infection.
#'
#' set.seed(999)
#' res <- res %>%
#'   group_by(Treatment) %>% # group the rows by Treatment
#'   mutate(Area = rnorm(n(), sample(1:10, 1), 1)) %>% # generate random data
#'   mutate(Area = if ( all(Treatment == "control") ) Area * 3 else Area + DAI*0.5) %>% # manipulate data
#'   ungroup() # remove the grouping
#' res
#' #'
#' #' Adding in Block information
#' #' ---------------------------
#' #'
#' #'
#' #' Because we will need to transform the data, we will need to include the block
#' block <- rep(1:12, each = 4)
#' block
#' res$Block <- block
#' res
#'
#' Reordering factors
#' ------------------
#'
#' The factors in the treatments will be out of order when plotting because it's
#' always alphabetical by default (and 1 comes before 2). Here we are reordering
#' the factors:
#'
unique(res$Treatment) # The correct order
res <- mutate(res, Treatment = factor(Treatment, levels = unique(Treatment)))
levels(res$Treatment)
#'
#' Calculationg Percent Disease Control
#' ====================================
#'
#' Percent disease control is "estimated as the difference in lesion area of the
#' control and treatment, divided by lesion area of the control and expressed as
#' percent." Because we will have to make this calculation many times, it's
#' best to write a function for it.
#'
#' Step 1: create a function
#' --------------------------
#'
#' Our function will take in two numbers, the lesion area of the control and
#' the lesion area of the treatment.
percent_control <- function(control, treatment){
  res <- (control - treatment)/control # estimate the disease control
  return(res*100)                      # express as percent
}
#'
#' We can show that this works:
percent_control(control = 10, treatment = 5)
#'
#' Step 2: spread the data in different columns by treatment
#' ---------------------------------------------------------
#'
#' Our data were recorded in a "tidy" fashion in which we had one observation
#' per row. In order to calculate the percent control, we'll need to rehshape
#' our data so that we have one column per treatment such that each row will
#' represent a single block per day after application.  We'll use the *tidyr*
#' function `spread()` to do this.
blocks <- res %>%
  select(DAI, Block, Treatment, Area) %>% # We don't need name or number here
  spread(Treatment, Area) # make new columns from treatment, and fill with Area
blocks
#'
#' Step 3: calculate percent control
#' ---------------------------------
#'
#' Now that we have reshaped our data, we can manipulate each treatment column
#' to give us the percent control.
# Note: the backtics "`" allow R to recognize a variable with spaces.
percents <- blocks %>%
  mutate(`2.53 mm water`  = percent_control(control, `2.53 mm water`)) %>%
  mutate(`5.07 mm water`  = percent_control(control, `5.07 mm water`)) %>%
  mutate(`10.13 mm water` = percent_control(control, `10.13 mm water`)) %>%
  mutate(control          = percent_control(control, 0))
percents
#' Because figure 2 plotted the average value, we want to summarize our data in
#' averages. To do this, we need to convert our data back to tidy format by
#' using the *tidyr* function `gather()`:
percents <- percents %>%
  gather(key = Treatment, value = Area, -DAI, -Block) %>%
  mutate(Treatment = factor(Treatment, levels = unique(Treatment))) # reset factor
percents
#'
#' Step 4: summarize the average lesion area
#' -----------------------------------------
#'
#' We can summarize the average area per DAI and Treatment, which will allow us
#' to plot the data in the manner of Morini *et al.* 2017.
avgs <- percents %>%
  group_by(DAI, Treatment) %>%
  summarize(meanArea = mean(Area)) %>%
  ungroup()
avgs
#'
#' Now that we have our averages, we can plot it using *ggplot2*
#+ fig.width = 6, fig.height = 4
ggplot(avgs, aes(x = DAI, y = meanArea, group = Treatment)) +
  geom_point(aes(pch = Treatment), size = 3) + # plot the points
  stat_smooth(aes(lty = Treatment), method = "lm", se = FALSE, color = "black") + # plot the regression
  theme_classic() + # change the appearance
  ylim(0, 100) + # set the limits on the y axis
  theme(text = element_text(size = 14)) + # increase the text size
  labs(list( # Set the labels
    x = "Days after fluazinam application",
    y = "Percent disease control",
    pch = "Irrigation levels",
    lty = "Irrigation levels"))
#'
#' Additional visualiztations
#' ==========================
#'
#' When we plot the averages, we inadvertently hide the data. Of course, if we
#' tried to plot all the data in one graph, it would look a bit messy:
#+ fig.width = 6, fig.height = 4
ggplot(percents, aes(x = DAI, y = Area, group = Treatment)) +
  geom_point(aes(pch = Treatment), size = 3) + # plot the points
  stat_smooth(aes(lty = Treatment), method = "lm", se = FALSE, color = "black") + # plot the regression
  theme_classic() + # change the appearance
  ylim(0, 100) + # set the limits on the y axis
  theme(text = element_text(size = 14)) + # increase the text size
  labs(list( # Set the labels
    x = "Days after fluazinam application",
    y = "Percent disease control",
    pch = "Irrigation levels",
    lty = "Irrigation levels"))
#'
#' With ggplot2, we can spread the data out into "facets":
#+ fig.width = 8, fig.height = 3
ggplot(percents, aes(x = DAI, y = Area, group = Treatment)) +
  geom_point(aes(pch = Treatment), size = 2) + # plot the points
  stat_smooth(aes(lty = Treatment), method = "lm", se = FALSE, color = "black") + # plot the regression
  theme_classic() + # change the appearance
  ylim(0, 100) + # set the limits on the y axis
  theme(text = element_text(size = 14)) + # increase the text size
  facet_wrap(~Treatment, nrow = 1) +
  theme(aspect.ratio = 1) +
  labs(list( # Set the labels
    x = "Days after fluazinam application",
    y = "Percent disease control",
    pch = "Irrigation levels",
    lty = "Irrigation levels"))
