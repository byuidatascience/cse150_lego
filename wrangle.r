pacman::p_load(tidyverse, downloader, fs, glue, magick, trelliscopejs)
pacman::p_load_gh("byuidss/DataPushR")

# https://www.kaggle.com/rtatman/lego-database has the lego-database.zip
# https://rebrickable.com/downloads/ has each csv from the database.
# 

# other ideas
# https://www.kaggle.com/joosthazelzet/lego-brick-images
# https://www.littlemissdata.com/blog/rlego

lego_paths <- fs::dir_ls(path = "../../../Downloads/lego-database/", regexp = ".csv")
lego_paths[c(1,5,6)]
# ../../../Downloads/lego-database/colors.csv          
# ../../../Downloads/lego-database/part_categories.csv 
#../../../Downloads/lego-database/parts.csv 
# lego_paths %>% map(~read_csv(file = .x))

lego_color <- read_csv(lego_paths[1])
lego_cat <- read_csv(lego_paths[5])
lego_part <- read_csv(lego_paths[6])

lego_picture_paths <- list.files(path = "../../byuidatascience/data4legos/data-raw/", pattern = ".png", recursive = TRUE, full.names = TRUE)

bricks <- lego_part %>%
  filter(part_cat_id %in% lego_cat$id[lego_cat$name %in% c("Bricks Printed", "Bricks", "Bricks Wedged", "Bricks Sloped")]) %>%
  filter(str_c(part_num, ".png") %in% path_file(lego_picture_paths))


### try to tidy the name column
bricks <- bricks %>%
  separate(name, into = c("beg", "description"), sep = " with | w/ ", remove = FALSE) %>%
  separate(beg, into = c("beg", "description_without"), sep = " without | w/o ") %>%
  mutate(description = ifelse(is.na(description), paste0("without ", description_without) %>%
                                str_remove_all("without NA"), 
                              paste0("without ", description_without," with ", description) %>%
                                str_remove_all("without NA ")),
         size = beg %>% str_replace_all("  ", " ") %>%
           str_extract_all("[:digit:]{1,2} x [:digit:]{1,2} x [:digit:]/[:digit:]|[:digit:]{1,2} x [:digit:]{1,2} x [:digit:]{1,2}|[:digit:]{1,2} x [:digit:]{1,2}") %>% unlist()) %>%
  separate(beg, into = c("type", "description_start"), sep = str_c(.$size[order(str_length(.$size), decreasing = TRUE)], collapse = "|")) %>%
  mutate(description = str_c(description_start, " ", description)) %>%
  select(-description_without, -description_start, size_name = size)  %>%
  separate(size_name, into = c("length", "width", "height"), sep = " x ", remove = FALSE, convert = TRUE) %>%
  mutate(height = sapply(.$height, function(x) eval(parse(text = x))) %>% round(2)) %>%
  select(part_num, type, description, length, width, height, size_name, part_cat_id) %>%
  mutate_if(is.character, str_trim)
write_csv(bricks, "data/all_lego_bricks.csv")


# pick a lego

rand_lego <- function(){
  rand_part <- sample(bricks$part_num, 1) %>% str_c(".png")
  
  part_df <- bricks %>%
    filter(part_num == rand_part)
  
  picked_lego_color_paths <- lego_picture_paths %>% 
    str_subset(rand_part) %>% sample(1)
  
  image <- picked_lego_color_paths %>%  
    image_read() %>%
    image_ggplot() %>%
    list()
  
  #image_resize(str_c(part_df$length, "00x", part_df$height,"00"))
  
  tibble(part = rand_part, path = picked_lego_color_paths, image = image)
}


build_samples_lego <- function(team, n_sample = 250){
  lego_bunch <- 1:n_sample %>% map(~rand_lego()) %>%
    bind_rows() 
  class(lego_bunch$image) <- c("trelliscope_panels", "list")
  dat_trello <- lego_bunch %>%
    mutate(color_id = str_remove_all(path, fs::path_common(path)) %>% 
             str_remove_all(part) %>%
             str_remove_all("//parts_|/") %>% as.numeric(),
           part = str_remove_all(part, ".png")) %>%
    select(part, color_id, image, -path) %>%
    left_join(select(lego_color, color_id = id, name, rgb)) %>%
    select(-color_id) %>%
    mutate(sample = 1:n())
  
  p <- trelliscope(select(dat_trello, sample, rgb, name, image), name = str_c("Team ", team), 
                   path = "../../byuidatascience/cse150_lego/docs", nrow = 3, ncol = 6, thumb = TRUE, 
                   desc = "A random sample of Lego blocks")
  print(p)
  dat_trello
  
}

sample_info <- str_pad(1:20, width = 2, side = "left", pad = "0") %>% map(~build_samples_lego(.x))

rand_lego()
