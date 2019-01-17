## ---- load
source("R/theme.R")
library(lubridate)
library(tidyverse)
library(tsibble)

elec_tbl <- read_rds("~/Research/paper-tsibble/data/smart-meter13.rds") %>% 
  select(customer_id, reading_datetime, general_supply_kwh, everything())
elec_tbl

## ---- coerce
elec_ts <- elec_tbl %>% 
  as_tsibble(
    index = reading_datetime,
    key = id(customer_id),
  )

## ---- print
elec_ts

## ---- line-na
customer_659 <- elec_ts %>% 
  filter(customer_id == 8156659)

customer_659 %>% 
  ggplot(aes(x = reading_datetime, y = general_supply_kwh)) +
  geom_line(size = 0.4) +
  theme_remark()

## ---- fill-gaps
customer_659 %>% 
  fill_gaps() %>% 
  ggplot(aes(x = reading_datetime, y = general_supply_kwh)) +
  geom_line(size = 0.4) +
  theme_remark()

## ---- has-gaps
print(has_gaps(elec_ts), n = 10)

## ---- index-by
elec_avg <- elec_ts %>% 
  index_by(datetime = floor_date(reading_datetime, "hour")) %>% #<<
  summarise(avg_kwh = mean(general_supply_kwh)) %>% 
  print()

## ---- slide-animate
elec_jan <- elec_avg %>% 
  filter_index(~ "2013-01-15")
library(gganimate)
slide_window <- slider(elec_jan$datetime, .size = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_slide <- ggplot() +
  geom_line(aes(datetime, avg_kwh), size = 1.2, data = elec_jan) +
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

## ---- sliding-average
reveal_group <- function(data, group){
  group <- enquo(group)
  data <- as_tibble(data)
  data <- transmute(
    data, !! group,
    .dt = map(map(!! group, seq_len), function(groups, data) {
      data <- filter(data, !! group %in% groups)
      select(data, !! expr(-!! group))
    }, data
  ))
  unnest(data, .dt)
}
elec_slide_mean <- elec_jan %>%
  mutate(ma_kwh24 = slide_dbl(avg_kwh, ~ mean(.x), .size = 24, .align = "cl")) %>%
  mutate(group = pmax(0, row_number() - 11))
elec_slide_revealed <- elec_slide_mean %>%
  reveal_group(group)
elec_slide_mean <- ggplot() +
  geom_line(aes(x = datetime, y = avg_kwh), data = elec_jan, colour = "grey", size = 1.2) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = slide_window, fill = "#9ecae1", alpha = 0.6) +
  geom_point(aes(x = datetime, y = ma_kwh24), data = elec_slide_mean, size = 2, colour = "#de2d26") +
  geom_line(aes(x = datetime, y = ma_kwh24), data = elec_slide_revealed, size = 1.2, colour = "#de2d26") +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/slide-mean.gif", elec_slide_mean, width = 800, height = 250)

## ---- tile-animate
tile_window <- tiler(elec_jan$datetime, .size = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_tile <- ggplot() +
  geom_line(aes(datetime, avg_kwh), data = elec_jan, size = 1.2) +
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

## ---- tile-average
elec_tile_mean <-
  tibble(
    datetime = make_datetime(2013, 1, 1:15, hour = 12),
    ma_kwh24 = tile_dbl(elec_jan$avg_kwh, ~ mean(.x), .size = 24)
  ) %>%
  mutate(group = row_number())
elec_tile_revealed <- elec_tile_mean %>%
  reveal_group(group)
elec_tile_mean <- ggplot() +
  geom_line(aes(x = datetime, y = avg_kwh), data = elec_jan, colour = "grey", size = 1.2) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = tile_window, fill = "#9ecae1", alpha = 0.6) +
  geom_point(aes(x = datetime, y = ma_kwh24), data = elec_tile_mean, size = 2, colour = "#de2d26") +
  geom_line(aes(x = datetime, y = ma_kwh24), data = elec_tile_revealed, size = 1.2, colour = "#de2d26") +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/tile-mean.gif", elec_tile_mean, width = 800, height = 250)

## ---- stretch-animate
stretch_window <- stretcher(elec_jan$datetime, .init = 24) %>%
  map_dfr(function(x) tibble(xmin = min(x), xmax = max(x))) %>%
  mutate(ymin = -Inf, ymax = Inf, group = row_number())
p_stretch <- ggplot() +
  geom_line(aes(datetime, avg_kwh), data = elec_jan, size = 1.2) +
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

## ---- stretch-average
elec_stretch_mean <- elec_jan %>%
  mutate(
    ma_kwh24 = c(rep(NA_real_, 23), stretch_dbl(avg_kwh, ~ mean(.x), .init = 24)),
    group = pmax(0, row_number() - 23)
  )
elec_stretch_revealed <- elec_stretch_mean %>%
  reveal_group(group)
elec_stretch_mean <- ggplot() +
  geom_line(aes(x = datetime, y = avg_kwh), data = elec_jan, colour = "grey", size = 1.2) +
  geom_rect(aes(
    xmin = xmin, xmax = xmax,
    ymin = ymin, ymax = ymax,
    group = group
  ), data = stretch_window, fill = "#9ecae1", alpha = 0.6) +
  geom_point(aes(x = datetime, y = ma_kwh24), data = elec_stretch_mean, size = 2, colour = "#de2d26") +
  geom_line(aes(x = datetime, y = ma_kwh24), data = elec_stretch_revealed, size = 1.2, colour = "#de2d26") +
  xlab("Time") +
  ylab("Average kwH") +
  ylim(c(0, max(elec_jan$avg_kwh))) +
  theme_bw() +
  transition_manual(group)
anim_save("img/stretch-mean.gif", elec_stretch_mean, width = 800, height = 250)

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
    style = cells_styles(text_font = "Monospace"),
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

## ---- moving-average
elec_ma <- elec_avg %>% 
  mutate(
    ma_kwh24 = slide_dbl(avg_kwh, ~ mean(.x), .size = 24, .align = "cl"),
    ma_kwh720 = slide_dbl(avg_kwh, ~ mean(.x), .size = 720, .align = "cl"),
  ) 

## ---- moving-average-plot
elec_ma %>% 
  ggplot(aes(x = datetime)) +
  geom_point(aes(y = avg_kwh), color = "grey60", size = 0.1, alpha = 0.6) +
  geom_line(aes(y = ma_kwh24), color = "#7570b3", size = 1.2) +
  geom_line(aes(y = ma_kwh720), color = "#d95f02", size = 1.2)

## ---- split-data
library(fable)
elec_jan <- elec_avg %>% 
  filter_index(~ "2013-01-30") %>% 
  mutate(date = as_date(datetime), hour = hour(datetime))
elec_jan31 <- elec_avg %>% 
  filter_index("2013-01-31")

## ---- calendar-train
weather <- read_rds("~/Research/paper-tsibble/data/weather13.rds") %>% 
  mutate(hot = if_else(max_temp >= 32, "Hot", "Not hot"))
elec_jan %>% 
  left_join(weather, by = "date") %>% 
  ggplot(aes(x = hour, y = avg_kwh, colour = hot)) +
  geom_line(size = 1.2) +
  sugrrants::facet_calendar(~ date) +
  scale_colour_manual(
    values = c("Hot" = "#de2d26", "Not hot" = "#000000"),
    guide = guides(colour = "none")
  ) +
  theme_remark()

## ----- model
elec_mbl <- elec_jan %>% 
  model(
    yesterday = NAIVE(avg_kwh ~ lag("1 day")),
    ets = ETS(avg_kwh)
  ) %>%
  print()

## ---- tidy
tidy(elec_mbl)

## ---- glance
glance(elec_mbl)

## ---- augment
augment(elec_mbl)

## ---- forecast
elec_fbl <- elec_mbl %>% 
  forecast(h = "1 day") %>% 
  print()

## ---- vis-naive
# autoplot(elec_fbl, data = elec_jan)
elec_ftf <- elec_fbl %>% 
  fortify() %>% 
  filter(.model == "yesterday") %>% 
  mutate(date = as_date(datetime), hour = hour(datetime))
ylims <- round(c(min(elec_ftf$lower), max(elec_avg$avg_kwh)), 2)
elec_jan %>% 
  ggplot(aes(x = hour, y = avg_kwh)) +
  geom_line(size = 1.2) +
  geom_forecast(
    aes(ymin = lower, ymax = upper, level = level),
    elec_ftf, stat = "identity", size = 1.2
  ) +
  sugrrants::facet_calendar(~ date) +
  ylim(ylims) +
  theme_remark()

# elec_jan %>% 
#   ggplot(aes(x = hour, y = avg_kwh)) +
#   geom_line(size = 1.2) +
#   geom_forecast(aes(dist = .distribution), data = elec_fbl) +
#   sugrrants::facet_calendar(~ date) +
#   theme_remark()

## ---- vis-ets
elec_ftf <- elec_fbl %>% 
  fortify() %>% 
  filter(.model == "ets") %>% 
  mutate(date = as_date(datetime), hour = hour(datetime))
elec_jan %>% 
  ggplot(aes(x = hour, y = avg_kwh)) +
  geom_line(size = 1.2) +
  geom_forecast(
    aes(ymin = lower, ymax = upper, level = level),
    elec_ftf, stat = "identity", size = 1.2
  ) +
  sugrrants::facet_calendar(~ date) +
  ylim(ylims) +
  theme_remark()

## ----- accuracy
accuracy(elec_fbl, elec_jan31)

## ---- pred-data
# autoplot(elec_fbl, level = 0, data = elec_jan31)
elec_jan31 %>% 
  ggplot(aes(x = datetime, y = avg_kwh)) +
  geom_line(size = 1.2) +
  geom_forecast(
    aes(ymin = lower, ymax = upper, colour = .model),
    fortify(elec_fbl, level = 0), stat = "identity", size = 1.2
  ) +
  scale_colour_brewer(palette = "Dark2")

## ----- subset
all_id <- has_gaps(elec_ts) %>% 
  filter(!.gaps) %>% 
  pull(customer_id)
elec_sub <- elec_ts %>% 
  filter(customer_id %in% all_id) %>% 
  filter_index(~ "2013-01-14") %>% 
  group_by_key() %>% 
  index_by(datetime = floor_date(reading_datetime, "hour")) %>%
  summarise(general_supply_kwh = sum(general_supply_kwh))
which_zero <- elec_sub %>% 
  filter(general_supply_kwh == 0) %>% 
  pull(customer_id) %>% 
  unique()
elec_sub <- elec_sub %>% 
  filter(!(customer_id %in% which_zero)) %>% 
  print()

## ---- batch-model
elec_mbl <- elec_sub %>% 
  model(ets = ETS(log(general_supply_kwh))) %>% 
  print(n = 10)

## ---- batch-forecast
elec_fct <- elec_mbl %>% 
  forecast(h = "1 day")

## ---- batch
elec_fct <- elec_sub %>% 
  model(ets = ETS(log(general_supply_kwh))) %>% 
  forecast(h = "1 day")

## ---- batch-plot
set.seed(20190112)
plot_id <- sample(unique(elec_sub$customer_id), size = 4)
elec_ftf <- elec_fct %>% 
  fortify() %>% 
  filter(customer_id %in% plot_id)
elec_sub %>% 
  filter(customer_id %in% plot_id) %>% 
  ggplot(aes(x = datetime, y = general_supply_kwh)) +
  geom_line(size = 1.2) +
  geom_forecast(
    aes(ymin = lower, ymax = upper, level = level),
    elec_ftf, stat = "identity", size = 1.2
  ) +
  facet_wrap(~ customer_id, scales = "free_y", ncol = 1, labeller = "label_both") +
  xlab("Reading time") +
  ylab("General supply kwH") +
  theme_remark()

# elec_sub %>% 
#   ggplot(aes(x = reading_datetime, y = general_supply_kwh)) +
#   geom_line() +
#   facet_wrap(~ customer_id, ncol = 1)

## ---- expand-forecast
library(future)
plan(multiprocess)
expand_forecast <- function(...) {
  as_tsibble(list(...), index = datetime, validate = FALSE) %>% 
    model(ets = ETS(avg_kwh)) %>% 
    forecast(h = "1 day")
}
subset_tsibble <- function(...) {
  as_tsibble(list(...), index = datetime, validate = FALSE)
}
elec_dat <- elec_jan %>% 
  pstretch(subset_tsibble, .size = 24, .init = 24 * 7)
elec_fct <- elec_jan %>% 
  future_pstretch(expand_forecast, .size = 24, .init = 168)

## ---- anim-forecast
p <- ggplot() 
for (j in 1:length(elec_dat)) {
  dat <- elec_dat[[j]]
  fct <- elec_fct[[j]]
  p <- p + geom_line(aes(x = datetime, y = avg_kwh), data = dat) +
  geom_forecast(aes(x = datetime, y = avg_kwh, 
    ymin = lower, ymax = upper, level = level),
    fct, stat = "identity"
  )
}
p_rolling <- p +
  xlab("Time") +
  ylab("Average kwH") +
  theme_bw() +
  transition_layers(
    layer_length = 1, transition_length = 2, 
    keep_layers = c(Inf, 0), from_blank = FALSE,
  )
anim_save("img/rolling.gif", p_rolling, width = 800, height = 250, nframes = 200)

## ---- expand-forecast-2
library(future)
plan(multiprocess)
subset_tsibble <- function(...) {
  as_tsibble(list(...), index = datetime, key = id(customer_id),
    validate = FALSE)
}
expand_forecast <- function(...) {
  as_tsibble(list(...), index = datetime, key = id(customer_id),
    validate = FALSE) %>% 
    model(ets = ETS(general_supply_kwh)) %>% 
    forecast(h = "1 day")
}
elec_lst <- elec_sub %>% 
  split(.$customer_id)
elec_dat <- elec_lst %>% 
  map(~ pstretch(., subset_tsibble, .size = 24, .init = 24 * 7))
elec_fct <- elec_lst %>% 
  map(~ future_pstretch(., expand_forecast, .size = 24, .init = 24 * 7))

## ---- anim-forecast-2
for (i in 1:4) {
  p <- ggplot() 
  for (j in 1:25) {
    dat <- elec_dat[[i]][[j]]
    fct <- elec_fct[[i]][[j]]
    p <- p + geom_line(aes(x = datetime, y = general_supply_kwh), data = dat) +
    geom_forecast(aes(x = datetime, y = general_supply_kwh, 
      ymin = lower, ymax = upper, level = level),
      fct, stat = "identity"
    )
  }
  p_rolling <- p +
    transition_layers(
      layer_length = 1, transition_length = 2, 
      keep_layers = c(Inf, 0), from_blank = FALSE,
    )
  file <- paste0("img/rolling", i, ".gif")
  anim_save(file, p_rolling, width = 800, height = 250)
}
