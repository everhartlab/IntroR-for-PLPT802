#' ---
#' title: Figure 2
#' output: html_document
#' ---
#'
#' Reading in Data and Example Analysis
#' ------------------------
#' 
#' In this exercise, we will learn how to read in data as an object in R, find and load 
#' packages into our library, and then re-create Figure 2 from the 2017 paper by Morini et al. 
#' paper "Control of white mold of dry bean and residual activity of fungicides applied
#' by chemigation." 
#' 
#' We can use the `read.table()` function to read these data in to R. It's
#' important to remember that while in R, these data are simply a copy kept *in
#' memory*, not on the disk, so we don't have to worry too much about
#' accidentally deleting the data :).
#' 
#' So, how do we actually USE the `read.table()` function? A good first step to
#' figuring out how you can use a function is to look at it's help page. The way
#' you can do that is by typing either help("function_name") or ?function_name.
#' 
#' 
#' 
     # Type ?read.table and answer these three questions:
     # 
     # 1. What does it do? (Description)
     # 2. What are the first three arguments and their defaults? (Usage/Arguments)
     # 3. What does it return? (Value)
#'
#' 
#'   In order to read our data into R, we will need to provide three things:
#' 
#'  1. The path to the data set                 : Results2017_combined-class.csv
#'  2. If the first row are column names        : yes
#'  3. The separator for each cell in the data  : comma
#' 
#' Now that we have these elements, we can read our data into a variable, which
#' we can call "res" because it is short for "results". Once we do this, we should 
#' check the dimensions to make sure that we have all of the data.
#'
#' Now we can read in the data and inspect to make sure it is what we expect.
res <- read.csv("Results2017_combined-class.csv", head = TRUE, stringsAsFactors = FALSE)
head(res)
dim(res)
#' This shows the data in columns named DAI, Number, Your.name, Area, and
#' Treatment.
#' 
#' 
# Thinking before doing: Your data and the analysis
# --------------------------------------------
# 
# The example data presented here were collected by students in the 2017
# PLPT802 class and should represent the same data that this year's class collected.
# This data consists of six columns of data: DAI, Number, Your.name, Area, Treatment, and Block.
# With these data, we want to answer the following questions:
# 
#  1. Can we convert lesion areas to a measurement that is relative to the control? 
#  2. What do we want to plot in order to demonstrate the residual disease control over
#     time by treatment?
# 
# To answer these questions, we will need to manipulate the layout of the data and will also
# need to write a function to perform a calculation and then plot the results of the 
# calculations over time.  Not all data manipulations will be described in detail here and
# are shown so that you can re-run this analysis using data collected in class.  We will
# do this using the following four steps:
#
#    1. Create a function
#    2. Spread the data in different columns by treatment
#    3. Calculate percent disease control
#    4. Summarize the average lesion area in a plot
#
#
#' Preparing Data and Packages
#' ===========================
#'
#' We will re-create the figure using three packages dplyr, tidyr, and ggplot2.  These packages
#' add flexibility to data and figure manipulations.  We will not go into these in detail.  Refer 
#' to the tab in this website called "Other Exercises" to get started learning more.
#' 
#+ package_loading, warning = FALSE, message = FALSE
library("dplyr")
library("tidyr")
library("ggplot2")
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
#' Calculating Percent Disease Control
#' ====================================
#'
#' Percent disease control is "estimated as the difference in lesion area of the
#' control and treatment, divided by lesion area of the control and expressed as
#' percent." Because we will have to make this calculation many times, it's
#' best to write a function for it.
#'
#' Step 1: Create a function
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
#' Step 2: Spread the data in different columns by treatment
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
#' Step 3: Calculate percent disease control
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
#' Step 4: Summarize the average lesion area
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
#' Additional visualizations
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
