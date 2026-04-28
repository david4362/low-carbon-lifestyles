###############################################################################
# CSV TABLE PNG HELPERS
#
# Utility functions to render CSV outputs as readable PNG tables.
###############################################################################

if (!requireNamespace("gridExtra", quietly = TRUE)) {
  stop("Package 'gridExtra' is required for PNG table rendering. Install with install.packages('gridExtra').")
}

format_table_column <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    return(as.character(x))
  }

  if (is.numeric(x)) {
    return(format(round(x, 4), trim = TRUE, scientific = FALSE))
  }

  as.character(x)
}

render_data_frame_to_png <- function(data,
                                     png_base_path,
                                     title,
                                     rows_per_page = 35,
                                     max_pages = 6) {
  if (nrow(data) == 0) {
    data <- data.frame(Note = "No rows", stringsAsFactors = FALSE)
  }

  page_starts <- seq(1, nrow(data), by = rows_per_page)
  total_pages <- length(page_starts)
  rendered_pages <- min(total_pages, max_pages)

  for (page_idx in seq_len(rendered_pages)) {
    row_start <- page_starts[page_idx]
    row_end <- min(nrow(data), row_start + rows_per_page - 1)

    page_data <- data[row_start:row_end, , drop = FALSE]
    page_data <- as.data.frame(
      lapply(page_data, format_table_column),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (rendered_pages == 1) {
      png_file <- png_base_path
    } else {
      png_file <- sub("\\.png$", paste0("_p", page_idx, ".png"), png_base_path)
    }

    width_px <- max(1800, 220 * max(1, ncol(page_data)))
    height_px <- max(950, 52 * (nrow(page_data) + 6))

    grDevices::png(filename = png_file, width = width_px, height = height_px, res = 150)

    table_grob <- gridExtra::tableGrob(
      page_data,
      rows = NULL,
      theme = gridExtra::ttheme_minimal(base_size = 9)
    )

    page_text <- if (total_pages > 1) {
      paste0(" — page ", page_idx, "/", total_pages)
    } else {
      ""
    }

    range_text <- paste0(" (rows ", row_start, "-", row_end, " of ", nrow(data), ")")

    title_grob <- grid::textGrob(
      paste0(title, page_text, range_text),
      x = 0,
      hjust = 0,
      gp = grid::gpar(fontsize = 13, fontface = "bold")
    )

    note_text <- if (total_pages > max_pages && page_idx == rendered_pages) {
      paste0("Showing first ", max_pages, " pages only.")
    } else {
      ""
    }

    note_grob <- grid::textGrob(
      note_text,
      x = 0,
      hjust = 0,
      gp = grid::gpar(fontsize = 9, col = "grey35")
    )

    layout <- gridExtra::arrangeGrob(
      title_grob,
      table_grob,
      note_grob,
      ncol = 1,
      heights = c(0.08, 0.88, 0.04)
    )

    grid::grid.newpage()
    grid::grid.draw(layout)
    grDevices::dev.off()
  }
}

render_csv_tables_to_png <- function(root_dir,
                                     recursive = TRUE,
                                     rows_per_page = 35,
                                     max_pages = 6) {
  csv_files <- list.files(root_dir, pattern = "\\.csv$", recursive = recursive, full.names = TRUE)

  if (length(csv_files) == 0) {
    message("No CSV files found for PNG rendering in: ", root_dir)
    return(invisible(character(0)))
  }

  rendered <- c()

  for (csv_path in csv_files) {
    csv_data <- tryCatch(
      read.csv(csv_path, check.names = FALSE, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (is.null(csv_data)) {
      message("Skipping unreadable CSV: ", csv_path)
      next
    }

    png_base_path <- sub("\\.csv$", ".png", csv_path)

    render_data_frame_to_png(
      data = csv_data,
      png_base_path = png_base_path,
      title = basename(csv_path),
      rows_per_page = rows_per_page,
      max_pages = max_pages
    )

    rendered <- c(rendered, csv_path)
  }

  message("Rendered ", length(rendered), " CSV table(s) to PNG in: ", root_dir)
  invisible(rendered)
}
