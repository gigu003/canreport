#' Insert multiple images into a Quarto document
#'
#' List image files from a directory and output them as Markdown image
#' links. Images can optionally be wrapped in a Quarto fenced layout
#' block, allowing flexible layout control (e.g. multi-column or custom
#' grids). This function is primarily designed for Quarto/Typst reports,
#' such as cancer registry quality control reports or annual statistical
#' reports.
#'
#' @param addr Character string. Path to the directory containing image files.
#'   Subdirectories are searched recursively.
#' @param fenced Logical. Whether to wrap the images in a Quarto fenced block.
#'   Default is `TRUE`.
#' @param layout Character string. Layout specification used inside the
#'   Quarto fenced block. The value is inserted verbatim as an attribute
#'   (e.g. `"layout-ncol=2"` for equal columns, or `"layout=[[3,2]]"` for a
#'   single-row grid with 3:2 proportion in Typst output). Default is
#'   `"layout-ncol=2"`.
#' @param attr Character string. Optional additional Markdown image attributes
#'   applied to every image (e.g. `"width=100%"`, `"height=20em"`). Default is
#'   `""` (no extra attributes).
#' @param ext Character vector. Image file extensions to include (without
#'   leading dots), such as `"jpg"`, `"png"`, or `"jpeg"`. Matching is
#'   performed using regular expressions.
#' @param sort Character string. Method used to sort images. One of:
#'   `"name"` (basename of file),
#'   `"mtime"` (modification time),
#'   `"ctime"` (change time),
#'   `"size"` (file size),
#'   or `"none"` (no sorting).
#' @param decreasing Logical. Whether to sort images in decreasing order.
#'   Default is `TRUE`.
#'
#' @returns
#' Invisibly returns a character vector of image file paths that were inserted.
#' The function is mainly called for its side effect of printing Markdown
#' content to the console or document.
#'
#' @note
#' For Typst output (PDF):
#' - Use `layout-ncol=N` for N equal columns.
#' - For non-equal proportions, use `layout="[[3,2]]"` (single row, 3:2 ratio),
#'   `layout="[[1,1],[2]]"` (two rows, etc.), or any valid Typst grid array.
#' - Invalid attributes (e.g. `layout-ncol="(3,2)"`) will cause rendering errors.
#'
#' @examples
#' \dontrun{
#' ## Default: two equal columns
#' quarto_insert_images("images/figure")
#'
#' ## Five equal columns (ideal for displaying book covers horizontally)
#' quarto_insert_images("images/books", layout = "layout-ncol=5")
#'
#' ## Non-uniform single-row layout (3:2 ratio) in Typst
#' quarto_insert_images("images/figure", layout = "layout=[[3,2]]")
#'
#' ## Custom grid: first row 70:30, second row full width
#' quarto_insert_images(
#'   "images/figure",
#'   layout = "layout=[[70,30],[100]]",
#'   attr = "width=95%"
#' )
#'
#' ## Sort by modification time (oldest first)
#' quarto_insert_images(
#'   "images/figure",
#'   sort = "mtime",
#'   decreasing = FALSE
#' )
#'
#' ## Plain images without fenced layout
#' quarto_insert_images("images/figure", fenced = FALSE)
#' }
#'
#' @export
quarto_insert_images <- function(
    addr,
    fenced = TRUE,
    layout = "layout-ncol=2",
    attr = "",
    ext = c("jpg", "png", "jpeg", "svg"),
    sort = c("name", "mtime", "ctime", "size", "none"),
    decreasing = TRUE
) {
  sort <- match.arg(sort)
  ext_pattern <- paste0("\\.(", paste(ext, collapse = "|"), ")$", collapse = "")
  img_files <- fs::dir_ls(
    addr,
    recurse = TRUE,
    type = "file",
    regexp = ext_pattern
  )

  if (length(img_files) == 0) {
    warning("No image files found in ", addr)
    return(invisible(character(0)))
  }

  ## ---- sorting (improved: name sorts by basename) ----
  if (sort != "none") {
    info <- fs::file_info(img_files)
    img_files <- switch(
      sort,
      name = img_files[order(fs::path_file(img_files), decreasing = decreasing)],
      mtime = img_files[order(info$modification_time, decreasing = decreasing)],
      ctime = img_files[order(info$change_time, decreasing = decreasing)],
      size = img_files[order(info$size, decreasing = decreasing)]
    )
  }

  ## ---- build Markdown image links ----
  if (nzchar(attr)) attr <- paste0("{", attr, "}")
  imgs <- paste0("![](", img_files, ")", attr, collapse = "\n\n")

  ## ---- output ----
  if (fenced) {
    cat(glue::glue("::: {{{layout}}}\n{imgs}\n:::\n"))
  } else {
    cat(imgs, "\n")
  }

  invisible(img_files)
}
