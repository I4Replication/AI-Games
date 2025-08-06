###############################################################################
# Markov-chain graph (single words) for focus-group transcripts
# – keeps only nouns & adjectives via udpipe
# – restricts to top 30 words for clarity
# – embeds a top-10 frequency table
###############################################################################

# 1. Packages -----------------------------------------------------------------
pkgs <- c("tidyverse", "udpipe", "stopwords", "here",
          "igraph", "ggraph", "viridisLite", "cowplot", "grid")
new <- pkgs[!pkgs %in% installed.packages()[ , "Package"]]
if (length(new) > 0) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# 2. Paths --------------------------------------------------------------------
root_path  <- here::here()
focus_path <- file.path(root_path, "focus groups")
out_dir    <- file.path(root_path, "output/figures")
if (!dir.exists(out_dir)) dir.create(out_dir)

# 3. Read & clean transcripts --------------------------------------------------
txt_files <- list.files(focus_path, "\\.txt$", full.names = TRUE)

clean_text <- map_chr(txt_files, function(path) {
  readr::read_lines(path) |>
    discard(~ str_detect(.x, "^\\s*$|^\\d+$|-->")) |>        # blanks / nums / timestamps
    map_chr(~ str_replace(.x, "^\\s*[^:]{1,40}:\\s*", "")) |># strip “Name: ”
    paste(collapse = " ")
})                                                   # one big string per file

# 4. Load / download udpipe model ------------------------------------------------
model_path <- udpipe_download_model(language = "english")$file_model
model      <- udpipe_load_model(model_path)

# 5. Annotation & word list ----------------------------------------------------
anno <- udpipe_annotate(model, x = paste(clean_text, collapse = " "))
anno <- as_tibble(anno)

content_words <- anno |>
  filter(upos %in% c("NOUN", "ADJ")) |>
  mutate(token = tolower(token)) |>
  select(doc_id, paragraph_id, sentence_id, token_id, token)

# 6. Word frequencies ----------------------------------------------------------
word_freq <- content_words |>
  count(token, sort = TRUE) |>
  rename(word = token, freq = n)

# 7. Build transitions ---------------------------------------------------------
tokens_ordered <- content_words |>
  arrange(doc_id, paragraph_id, sentence_id, token_id) |>
  pull(token)

transitions <- tibble(
  from = head(tokens_ordered, -1),
  to   = tail(tokens_ordered, -1)
) |>
  count(from, to, sort = TRUE) |>
  rename(weight = n)

# 8. Limit to top 30 words for readability -------------------------------------
top_words <- word_freq |> slice_max(freq, n = 30) |> pull(word)

edges <- transitions |>
  filter(from %in% top_words & to %in% top_words)

nodes <- word_freq |> filter(word %in% top_words)

# 9. Create igraph object ------------------------------------------------------
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

# 10. Plot with ggraph ---------------------------------------------------------
set.seed(123)
p_graph <- ggraph(g, layout = "fr") +                       # force-directed layout
  geom_edge_link(aes(width = weight),
                 arrow = arrow(length = unit(3, "mm")),
                 end_cap = circle(2, "mm"),
                 alpha = 0.6,
                 colour = viridisLite::viridis(1, begin = 0.1, end = 0.4)) +
  scale_edge_width(range = c(0.3, 2), guide = "none") +
  geom_node_point(aes(size = freq, colour = freq)) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_size(range = c(3, 12), guide = "none") +
  scale_colour_viridis_c(option = "D", end = 0.85, guide = "none") +
  theme_void()

# 11. Build top-10 table text ---------------------------------------------------
top10 <- word_freq |> slice_max(freq, n = 10)
top10_lbl <- paste(sprintf("%-12s %5d", top10$word, top10$freq), collapse = "\n")

# 12. Overlay frequency table ---------------------------------------------------
p_final <- ggdraw(p_graph) +
  draw_grob(
    grid::rectGrob(gp = gpar(fill = "white", col = NA, alpha = 0.85)),
    x = 0.885, y = 0.135, width = 0.23, height = 0.25,
    hjust = 0.5, vjust = 0.5
  ) +
  draw_text(
    text = top10_lbl,
    x = 0.97, y = 0.04,
    hjust = 1, vjust = 0,
    fontfamily = "mono", size = 8,
    lineheight = 1.05
  )

# 13. Save & display -----------------------------------------------------------
out_png <- file.path(out_dir, "markov_words_focus_groups.png")
ggsave(out_png, p_final, width = 8, height = 6, dpi = 320, bg = "white")
print(p_final)

###############################################################################
# End of script
###############################################################################