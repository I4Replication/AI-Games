###############################################################################
# Markov-chain graph (overlapping 2-grams) for focus-group transcripts
# – keeps nouns & adjectives only (udpipe)
# – shows top 30 bigrams
# – embeds top-10 bigram frequency table
###############################################################################

# 1. Packages -----------------------------------------------------------------
pkgs <- c("tidyverse", "udpipe", "here", "igraph", "ggraph",
          "viridisLite", "cowplot", "grid")
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
    discard(~ str_detect(.x, "^\\s*$|^\\d+$|-->")) |>        # blank / nums / timestamps
    map_chr(~ str_replace(.x, "^\\s*[^:]{1,40}:\\s*", "")) |># strip “Name: ”
    paste(collapse = " ")
})

# 4. Load / download udpipe model ---------------------------------------------
model_path <- udpipe_download_model(language = "english")$file_model
model      <- udpipe_load_model(model_path)

# 5. Annotation & token list ---------------------------------------------------
anno <- udpipe_annotate(model, x = paste(clean_text, collapse = " "))
anno <- as_tibble(anno)

tokens <- anno |>
  filter(upos %in% c("NOUN", "ADJ")) |>
  mutate(token = tolower(token)) |>
  arrange(doc_id, paragraph_id, sentence_id, token_id) |>
  pull(token)

# 6. Build overlapping bigrams & counts ---------------------------------------
bigrams <- paste(head(tokens, -1), tail(tokens, -1))

bigram_freq <- tibble(bigram = bigrams) |>
  count(bigram, sort = TRUE)

# 7. Transitions between consecutive bigrams -----------------------------------
bigram_trans <- tibble(
  from = head(bigrams, -1),
  to   = tail(bigrams, -1)
) |>
  count(from, to, sort = TRUE) |>
  rename(weight = n)

# 8. Keep top 30 bigrams for clarity -------------------------------------------
top_bigrams <- bigram_freq |> slice_max(n, n = 30) |> pull(bigram)

edges <- bigram_trans |>
  filter(from %in% top_bigrams & to %in% top_bigrams)

nodes <- bigram_freq |>
  filter(bigram %in% top_bigrams) |>
  rename(name = bigram, freq = n)

# 9. Build igraph & plot -------------------------------------------------------
g <- graph_from_data_frame(edges, vertices = nodes, directed = TRUE)

set.seed(123)
p_graph <- ggraph(g, layout = "fr") +
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

# 10. Top-10 bigram table -------------------------------------------------------
top10 <- bigram_freq |> slice_max(n, n = 10)
top10_lbl <- paste(sprintf("%-20s %5d", top10$bigram, top10$n), collapse = "\n")

# 11. Overlay table ------------------------------------------------------------
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

# 12. Save & display -----------------------------------------------------------
out_png <- file.path(out_dir, "markov_bigrams_focus_groups.png")
ggsave(out_png, p_final, width = 8, height = 6, dpi = 320, bg = "white")
print(p_final)

###############################################################################
# End of script
###############################################################################