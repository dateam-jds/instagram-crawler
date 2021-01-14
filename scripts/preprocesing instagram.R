library(jsonlite)
library(tidyverse)
library(listviewer)

jds_raw <- read_json("C:/Users/WINDOWS X/Instascraper/pikobar_jabar/pikobar_jabar.json", simplifyVector = TRUE)

jds_tbl <-
  jds_raw %>% 
  pluck(1) %>%
  jsonlite::flatten() %>% 
  transpose() %>% 
  enframe(name = "post_id", value = "post") %>% 
  mutate(
    post_id = str_pad(post_id, width = max(nchar(post_id)), pad = "0"),
    post_id = paste0("ig", post_id)
  )

jds_extracted <-
  jds_tbl %>% 
  mutate(
    type = map_chr(post, "__typename"),
    timestamp = map_int(post, "taken_at_timestamp"),
    caption = map_chr(post, list("edge_media_to_caption.edges", "node", "text")),
    hashtags = map(post, "tags"),
    is_video = map_lgl(post, "is_video"),
    video_views = map_int(post, "video_view_count"),
    n_likes = map_int(post, "edge_media_preview_like.count"),
    comments_username = map(post, list("comments.data", "owner", "username")),
    comments_timestamp = map(post, list("comments.data", "created_at")),
    comments_text = map(post, list("comments.data", "text"))
  )


jds_extracted

jds_posts <-
  jds_extracted %>%
  mutate(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    caption = caption %>% str_remove_all("\\n") %>% str_trim(),
    n_hashtags = map_int(hashtags, length),
    hashtags = map_chr(hashtags, ~ paste(.x, collapse = ", ")),
    n_comments = map_int(comments_text, length),
    n_uniq_commenters = map_int(comments_username, ~.x %>% 
                                  as.character() %>% 
                                  n_distinct())
  ) %>% 
  select(-post, -starts_with("comments"), starts_with("n")) %>% 
  relocate(n_hashtags, .after = hashtags)
# select(id, timestamps, is_video, caption, tags, n_tags, everything(), -post)
View(jds_posts)

jds_posts %>% 
  ggplot(aes(timestamp, n_uniq_commenters)) +
  geom_line() +
  scale_y_log10()

jds_posts %>% 
  ggplot(aes(n_uniq_commenters, n_comments)) +
  geom_point()
scale_x_log10() +
  scale_y_log10()


jds_comments %>% 
  count(username, post_id, sort = TRUE)


jda_comment=jds_extracted %>%
  separate_rows(comments_username,comments_timestamp,comments_text)
  mutate(
    timestamp = as.POSIXct(timestamp, origin = "1970-01-01"),
    caption = caption %>% str_remove_all("\\n") %>% str_trim(),
    n_hashtags = map_int(hashtags, length),
    hashtags = map_chr(hashtags, ~ paste(.x, collapse = ", ")),
    n_comments = map_int(comments_text, length),
    n_uniq_commenters = map_int(comments_username, ~.x %>% 
                                  as.character() %>% 
                                  n_distinct())
  ) %>% 
  select(-post, -starts_with("comments"), starts_with("n")) %>% 
  relocate(n_hashtags, .after = hashtags)

  
jds_comment=jds_extracted %>%
  select(post_id,comments_username,comments_timestamp,comments_text) %>%
  unnest(c(comments_username,comments_timestamp,comments_text))

jds_comment=jds_comment %>%
  mutate(comments_timestamp = as.POSIXct(comments_timestamp, origin = "1970-01-01"),.keep ="unused")
