## ---- load
source("R/theme.R")
library(lubridate)
library(tidyverse)
library(tsibble)

elec_tbl <- read_rds("~/Research/paper-tsibble/data/smart-meter13.rds")
elec_tbl

## ---- coerce
elec_ts <- elec_tbl %>% 
  as_tsibble(
    key = id(customer_id), #<<
    index = reading_datetime #<<
  )

## ---- print
elec_ts

## ---- line-na
customer_659 <- elec_ts %>% 
  filter(customer_id == 8156659)

customer_659 %>% 
  ggplot(aes(x = reading_datetime, y = general_supply_kwh)) +
  geom_line(size = 0.4)

## ---- fill-gaps
customer_659 %>% 
  fill_gaps() %>% 
  ggplot(aes(x = reading_datetime, y = general_supply_kwh)) +
  geom_line(size = 0.4)

## ---- index-by
elec_avg <- elec_ts %>% 
  index_by(datetime = floor_date(reading_datetime, "hour")) %>% #<<
  summarise(avg_kwh = mean(general_supply_kwh)) %>% 
  print()

## ---- jan
elec_jan <- elec_avg %>% 
  filter_index(~ "2013-01-14")

## ---- slide-animate
library(gganimate)
slide_window <- slider(elec_jan$datetime, .size = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_slide <- ggplot() +
  geom_line(aes(datetime, avg_kwh), data = elec_jan) +
  geom_point(aes(datetime, avg_kwh), data = elec_jan, size = 0.5) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = slide_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/slide.gif", p_slide, width = 800, height = 250)

## ---- tile-animate
tile_window <- tiler(elec_jan$datetime, .size = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_tile <- ggplot() +
  geom_line(aes(datetime, avg_kwh), data = elec_jan) +
  geom_point(aes(datetime, avg_kwh), data = elec_jan, size = 0.5) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = tile_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/tile.gif", p_tile, width = 800, height = 250)

## ---- stretch-animate
stretch_window <- stretcher(elec_jan$datetime, .init = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_stretch <- ggplot() +
  geom_line(aes(datetime, avg_kwh), data = elec_jan) +
  geom_point(aes(datetime, avg_kwh), data = elec_jan, size = 0.5) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = stretch_window, fill = "#9ecae1", alpha = 0.6) +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/stretch.gif", p_stretch, width = 800, height = 250)

## ---- window-table
library(gt)
slide <- c("slide", "slide2", "pslide")
tile <- c("tile", "tile2", "ptile")
stretch <- c("stretch", "stretch2", "pstretch")
suffix <- c("()", "_dbl()", "_int()", "_lgl()", "_chr()")
type <- c("list", "double", "integer", "logical", "character")

window_tbl <- 
  tibble(
    type = map(type, ~ rep(.x, each = 3)),
    slide = map(suffix, ~ paste0(slide, .x)),
    tile = map(suffix, ~ paste0(tile, .x)),
    stretch = map(suffix, ~ paste0(stretch, .x))
  ) %>% 
  unnest()

window_tbl %>% 
  group_by(type) %>% 
  gt() %>% 
  tab_style(
    style = cells_styles( text_font = "Monospace"),
    locations = cells_data()
  )

# tibble(
#   slide = paste0(slide, suffix[1]),
#   tile = paste0(tile, suffix[1]),
#   stretch = paste0(stretch, suffix[1])
# ) %>% 
# unnest() %>% 
# gt() %>% 
# tab_style(
#   style = cells_styles(text_font = "Monospace", text_size = 12),
#   locations = cells_data()
# )

## ---- split-data
library(fable)
elec_jan <- elec_avg %>% 
  filter_index(~ "2013-01-30")
elec_jan31 <- elec_avg %>% 
  filter_index("2013-01-31")

elec_mbl <- elec_jan %>% 
  model(naive = NAIVE(avg_kwh), est = ETS(avg_kwh)) %>%
  print()

elec_fc <- elec_mbl %>% 
  forecast(h = 24) %>% 
  print()

autoplot(elec_fc, data = elec_jan)
accuracy(elec_fc, elec_jan31)
