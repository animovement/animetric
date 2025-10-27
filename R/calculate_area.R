
calculate_area_ <- function(data, as_aniframe = FALSE) {

}


#' Convert a key‑point data.frame to an sf object of polygons (one per time)
#'
#' @param df A data.frame (or tibble) that contains at least the columns
#'   `time`, `x`, `y`.  Any other columns are ignored for the geometry step.
#' @param order_fun Optional function that orders the points for each time
#'   before they are turned into a polygon.  By default the points are taken
#'   in the order they appear in the data.  Supplying `grDevices::chull`
#'   (or any function returning a permutation of row indices) will give a
#'   convex‑hull ordering, which is handy when the raw order is not already
#'   clockwise/anticlockwise.
#' @return An `sf` object with one row per distinct `time`.  The geometry
#'   column is of class `POLYGON` and can be fed directly to `st_area()`.
#' @examples
#' ## Using the snippet you posted (saved as `df`)
#' polys <- df_to_sf_polygons(df)
#' sf::st_area(polys)   # vectorised area for every time point
#' @export
df_to_sf_polygons <- function(df, order_fun = NULL) {
  # ------------------------------------------------------------------
  # 1. Basic checks ---------------------------------------------------
  # ------------------------------------------------------------------
  req <- c("time", "x", "y")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) {
    stop(
      sprintf("Missing required column(s): %s", paste(miss, collapse = ", ")),
      call. = FALSE
    )
  }

  # ------------------------------------------------------------------
  # 2. Load sf (only when needed) ------------------------------------
  # ------------------------------------------------------------------
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required for df_to_sf_polygons().", call. = FALSE)
  }

  # ------------------------------------------------------------------
  # 3. Build polygons -------------------------------------------------
  # ------------------------------------------------------------------
  geom_list <- df |>
    # Keep only the columns we need
    dplyr::select(c(time, x, y))
    # Split by time – each element is a data.frame for one timestamp
    split(geom_list$time) #|>
    # Turn each split into a POLYGON geometry
    lapply(function(sub_df) {
      # Re‑order points if the user supplied a function (e.g. convex hull)
      if (!is.null(order_fun)) {
        ord_idx <- order_fun(as.matrix(sub_df[, c("x", "y")]))
        sub_df <- sub_df[ord_idx, , drop = FALSE]
      }
      # Close the ring (first point repeated at the end)
      coords <- rbind(
        as.matrix(sub_df[, c("x", "y")]),
        as.matrix(sub_df[1, c("x", "y")])
      )
      sf::st_polygon(list(coords))
    })

  # ------------------------------------------------------------------
  # 4. Assemble into an sf object ------------------------------------
  # ------------------------------------------------------------------
  # First create a plain data.frame that holds the time column and the
  # geometry list‑column.
  tmp_df <- data.frame(
    time = as.numeric(names(geom_list)),
    geometry = sf::st_sfc(geom_list, crs = NA),
    stringsAsFactors = FALSE
  )

  # Order rows by time (this is where the previous pipe‑anonymous‑function
  # caused the error).  Doing it in a separate step sidesteps the limitation.
  tmp_df <- tmp_df[order(tmp_df$time), ]

  # Finally turn the data.frame into an sf object.
  sf::st_as_sf(tmp_df)
}
