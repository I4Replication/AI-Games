###############################################################################
# Word-cloud para transcripciones de focus-group
# – solo sustantivos y adjetivos con udpipe
# – top 400 palabras
# – paleta apta para daltónicos
# – recuadro con top-10 palabras y sus cuentas
# Ejecuta este script desde la raíz del proyecto
###############################################################################

# 1. Paquetes ------------------------------------------------------------------
pkgs <- c("tidyverse", "tidytext", "ggwordcloud", "viridisLite",
          "stopwords", "here", "cowplot", "grid", "udpipe")
new <- pkgs[!pkgs %in% installed.packages()[ , "Package"]]
if (length(new) > 0) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# 2. Rutas ---------------------------------------------------------------------
root_path  <- here::here()
focus_path <- file.path(root_path, "focus groups")
out_dir    <- file.path(root_path, "output/figures")
if (!dir.exists(out_dir)) dir.create(out_dir)

# 3. Leer transcripciones y limpiar encabezados --------------------------------
txt_files <- list.files(focus_path, "\\.txt$", full.names = TRUE)

clean_text <- map_chr(txt_files, function(path) {
  readr::read_lines(path) |>
    discard(~ str_detect(.x, "^\\s*$|^\\d+$|-->")) |>        # líneas en blanco, números, timestamps
    map_chr(~ str_replace(.x, "^\\s*[^:]{1,40}:\\s*", "")) |># quita “Nombre: ”
    paste(collapse = " ")
})

# 4. Cargar o descargar el modelo udpipe ---------------------------------------
model_path <- udpipe_download_model(language = "english")$file_model
model      <- udpipe_load_model(model_path)

# 5. Anotar texto y filtrar POS -------------------------------------------------
anno <- udpipe_annotate(model, x = paste(clean_text, collapse = " "))
anno <- as_tibble(anno)

content_words <- anno |>
  filter(upos %in% c("NOUN", "ADJ")) |>    # solo sustantivos y adjetivos
  mutate(token = tolower(token)) |>
  count(token, sort = TRUE) |>
  rename(word = token, n = n)

# 6. Preparar datos para la nube y el recuadro ---------------------------------
wc_plot_df <- content_words |> slice_max(n, n = 400)          # top-400

top10     <- content_words |> slice_max(n, n = 10)
top10_lbl <- paste(sprintf("%-12s %5d", top10$word, top10$n), collapse = "\n")

# 7. Construir la nube de palabras --------------------------------------------
set.seed(123)
p_wc <- ggplot(wc_plot_df,
               aes(label = word, size = n, colour = n)) +
  geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 22) +
  scale_colour_viridis_c(end = 0.85, guide = "none") +
  theme_void()

# 8. Superponer recuadro blanco y texto ----------------------------------------
p_final <- ggdraw(p_wc) +
  draw_grob(
    grid::rectGrob(gp = gpar(fill = "white", col = NA, alpha = 0.85)),
    x = 0.885, y = 0.135, width = 0.23, height = 0.25,
    hjust = 0.5, vjust = 0.5
  ) +
  draw_text(
    text       = top10_lbl,
    x = 0.97,  y = 0.04,
    hjust = 1, vjust = 0,
    fontfamily = "mono", size = 8,
    lineheight = 1.05
  )

# 9. Guardar y mostrar ---------------------------------------------------------
if (!dir.exists(out_dir)) dir.create(out_dir)
out_png <- file.path(out_dir, "wordcloud_focus_groups.png")
ggsave(out_png, p_final, width = 8, height = 6, dpi = 320, bg = "white")
print(p_final)

###############################################################################
# Fin del script
###############################################################################