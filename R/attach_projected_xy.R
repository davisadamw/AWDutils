
#' Attach reprojected XY columns to tibble with coordinates for point data
#'
#' @param data data.frame / tibble with columns containing lat/lon or projected XY coordinates.
#' @param coords in case of point data: names or numbers of the numeric columns holding coordinates
#' @param from_crs coordinate reference system of the current coords columns, integer with EPSG code, or character with proj4string ... something suitable as input to \link[sf]{st_crs}
#' @param to_crs desired coordinate reference system for new coords columns,integer with EPSG code, or character with proj4string ... something suitable as input to \link[sf]{st_crs}
#' @param to_coords names for the new columns of reprojected coordinates; if `remove = FALSE`, these should be different from `coords`
#' @param remove logical; remove the original coords columns from the data?
#'
#' @return A tibble with all the non-spatial columns in `data` plus two new columns containing reprojected point coordinates.
#' @export
#'
#' @examples
#'
attach_projected_cols <- function(data, coords, from_crs, to_crs,
                                  to_coords = c("X", "Y"), remove = TRUE) {
  sf_data <- data %>%
    sf::st_as_sf(coords = coords, crs = from_crs, remove = remove) %>%
    sf::st_transform(to_crs)

  coord_cols <- tibble::as_tibble(sf::st_coordinates(sf_data))
  names(coord_cols) <- to_coords

  sf_data %>%
    sf::st_drop_geometry() %>%
    dplyr::bind_cols(coord_cols)
}
