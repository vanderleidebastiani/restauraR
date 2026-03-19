#' @title Compose an UpSet plot
#' @encoding UTF-8
#' @description Modified version of the `upset` function of the `ComplexUpset` package with minimal code correction to work with `ggplot2` 4+
#' @import ComplexUpset
#' @importFrom utils modifyList head tail
#' @importFrom ggplot2 ggplot aes coord_flip xlab ylab guide_legend is_ggplot
#' @importFrom ggplot2 scale_color_manual scale_x_discrete scale_y_discrete scale_y_reverse scale_y_continuous
#' @importFrom ggplot2 geom_point geom_segment layer
#' @importFrom rlang quo_name
#' @importFrom ggplot2 theme theme_minimal element_blank geom_segment
#' @importFrom patchwork plot_layout plot_spacer guide_area wrap_elements
#' @param data a dataframe including binary columns representing membership in classes
#' @param intersect which columns should be used to compose the intersection
#' @param base_annotations a named list with default annotations (i.e. the intersection size barplot)
#' @param name the label shown below the intersection matrix
#' @param annotations a named list of annotations, each being a list with: `list(aes=mapping, geom=geom or list of geoms)`;
#'  * (optional) `highlight_geom=list of geoms` geoms which can be highlighted with queries,
#'  * (optional) `top_geom=list of geoms` which should show up on top of highlighted queries.
#' @param themes a named list of themes for components and annotations, see `upset_default_themes()`/`upset_modify_themes()`
#' @param stripes specification of the stripes appearance created with `upset_stripes()`
#' @param labeller function modifying the names of the sets (rows in the matrix)
#' @param height_ratio ratio of the intersection matrix to intersection size height
#' @param width_ratio ratio of the overall set size width to intersection matrix width
#' @param wrap whether the plot should be wrapped into a group (makes adding a tile/combining with other plots easier)
#' @param set_sizes the overall set sizes plot, e.g. from `upset_set_size()` (`FALSE` to hide)
#' @param mode region selection mode for computing the number of elements in intersection fragment. See `get_size_mode()` for accepted values.
#' @param queries a list of queries generated with `upset_query()`
#' @param guides action for legends aggregation and placement ('keep', 'collect', 'over' the set sizes)
#' @param encode_sets whether set names (column in input data) should be encoded as numbers (set to TRUE to overcome R limitations of max 10 kB for variable names for datasets with huge numbers of sets); default TRUE for upset() and FALSE for upset_data().
#' @param matrix the intersection matrix plot
#' @param ... Arguments passed on to `upset_data`
#' @keywords InternalFunction
upset <- function(data,
                  intersect,
                  base_annotations = 'auto',
                  name = 'group',
                  annotations = list(),
                  themes = upset_themes(),
                  stripes = upset_stripes(),
                  labeller = identity,
                  height_ratio = 0.5,
                  width_ratio = 0.3,
                  wrap = FALSE,
                  set_sizes = ComplexUpset::upset_set_size(),
                  mode = 'distinct',
                  queries = list(),
                  guides = NULL,
                  encode_sets = TRUE,
                  matrix = ComplexUpset::intersection_matrix(),
                  ...) {
  # @importFrom ComplexUpset extract_stat_params upset_set_size intersection_matrix check_argument solve_mode
  # @importFrom ComplexUpset intersection_size upset_data intersect_queries queries_for group_by_queries set_queries get_highlights_data merge_rows
  # @importFrom ComplexUpset segment_end highlight_layer matrix_background_stripes get_scale scale_if_missing get_mode_presence add_highlights_to_geoms
  `*.gg` <- function(a, b) {
    a_params <- c(a$aes_params, a$geom_params, ComplexUpset:::extract_stat_params(a))
    b_params <- c(b$aes_params, b$geom_params, ComplexUpset:::extract_stat_params(b))
    if (is.null(a_params)) {
      params <- b_params
    } else if (is.null(b_params)) {
      params <- a_params
    } else {
      params <- utils::modifyList(a_params,
                                  b_params
      )
    }
    if (length(a$data)) {
      data <- a$data
    } else if (length(b$data)) {
      data <- b$data
    } else {
      data <- NULL
    }
    if (is.null(b$mapping)) {
      mapping <- a$mapping
    } else if (is.null(a$mapping)) {
      mapping <- b$mapping
    } else {
      mapping <- utils::modifyList(a$mapping, b$mapping)
    }
    ggplot2::layer(geom = a$geom,
                   params = params,
                   stat = a$stat,
                   data = data,
                   mapping = mapping,
                   position = a$position
    )
  }
  if (!is.null(guides)) {
    ComplexUpset:::check_argument(guides, allowed = c('keep', 'collect', 'over'), 'guides')
  }
  mode <- ComplexUpset:::solve_mode(mode)
  if (inherits(base_annotations, 'character')) {
    if (base_annotations != 'auto') {
      stop('Unsupported value for `base_annotations`: provide a named list, or `"auto"`')
    } else {
      base_annotations <- list('Intersection size' = ComplexUpset::intersection_size(counts = TRUE, mode = mode)
      )
    }
  }
  # for backwards compatibility pre 1.2
  if (!inherits(stripes, 'upset_stripes')) {
    stripes <- upset_stripes(colors=stripes)
  }
  annotations <- c(annotations, base_annotations)
  data <- ComplexUpset::upset_data(data, 
                                    intersect, 
                                    mode = mode, 
                                    encode_sets = encode_sets, 
                                    ...)
  intersections_sorted <- rev(data$sorted$intersections)
  intersections_limits <- intersections_sorted[intersections_sorted %in% data$plot_intersections_subset]
  scale_intersections <- ggplot2::scale_x_discrete(limits = intersections_limits)
  sets_limits <- data$sorted$groups[data$sorted$groups %in% data$plot_sets_subset]
  show_overall_sizes <- !(inherits(set_sizes, 'logical') && set_sizes == FALSE)
  matrix_intersect_queries <- ComplexUpset:::intersect_queries(ComplexUpset:::queries_for(queries, 
                                                                                          'intersections_matrix'), 
                                                               data)
  matrix_group_by_queries <- ComplexUpset:::group_by_queries(ComplexUpset:::queries_for(queries, 
                                                                                        'intersections_matrix'), 
                                                             data$sanitized_labels)
  matrix_set_queries <- ComplexUpset:::set_queries(ComplexUpset:::queries_for(queries, 
                                                                              'intersections_matrix'), 
                                                   data$sanitized_labels)
  intersection_query_matrix <- ComplexUpset:::get_highlights_data(data$matrix_frame, 
                                                                  'intersection', 
                                                                  matrix_intersect_queries)
  group_query_matrix <- ComplexUpset:::get_highlights_data(data$matrix_frame, 
                                                           'group_by_group', 
                                                           matrix_group_by_queries)
  set_query_matrix <- ComplexUpset:::get_highlights_data(data$matrix_frame, 
                                                         'group', 
                                                         matrix_set_queries)
  query_matrix <- ComplexUpset:::merge_rows(ComplexUpset:::merge_rows(intersection_query_matrix,
                                                                      group_query_matrix),
                                            set_query_matrix)
  query_matrix <- query_matrix[query_matrix$value == TRUE, ]
  matrix_frame <- data$matrix_frame[data$matrix_frame$group %in% data$plot_sets_subset, ]
  intersections_matrix <- matrix + matrix_frame
  point_geom <- intersections_matrix$geom
  if (!is.null(point_geom$aes_params$size)) {
    dot_size <- point_geom$aes_params$size
  } else {
    dot_size <- 1
  }
  geom_layers <- c(
    # the dots outline
    list(intersections_matrix$geom * ggplot2::geom_point(
      color = ifelse(
        matrix_frame$value,
        intersections_matrix$outline_color$active,
        intersections_matrix$outline_color$inactive
      ),
      size = dot_size * 7/6,
      na.rm = TRUE
    )),
    # the dot
    list(intersections_matrix$geom * ggplot2::geom_point(
      # ggplot2::aes(color = value),
      ggplot2::aes(color = value),
      size = dot_size,
      na.rm = TRUE
    )),
    # interconnectors on the dots
    list(intersections_matrix$segment * ggplot2::geom_segment(ggplot2::aes(x = intersection,
                                                                           xend = intersection,
                                                                           y = ComplexUpset:::segment_end(matrix_frame, 
                                                                                                          data, 
                                                                                                          intersection, 
                                                                                                          utils::head),
                                                                           yend = ComplexUpset:::segment_end(matrix_frame, 
                                                                                                             data, 
                                                                                                             intersection, 
                                                                                                             utils::tail)
    ), 
    na.rm = TRUE)
    ),
    # highlighted interconnectors
    ComplexUpset:::highlight_layer(intersections_matrix$segment,
                                   geom_segment,
                                   query_matrix,
                                   args = list(ggplot2::aes(x = intersection,
                                                            xend = intersection,
                                                            y = ComplexUpset:::segment_end(query_matrix, 
                                                                                           data, 
                                                                                           intersection, 
                                                                                           utils::head),
                                                            yend = ComplexUpset:::segment_end(query_matrix, 
                                                                                              data, 
                                                                                              intersection, 
                                                                                              utils::tail)),
                                               na.rm = TRUE
                                   )
    ),
    # the highlighted dot
    ComplexUpset:::highlight_layer(intersections_matrix$geom,
                                   geom_point,
                                   query_matrix,
                                   args = list(size = dot_size)
    )
  )
  intersections_matrix$layers <- c(ComplexUpset:::matrix_background_stripes(data, 
                                                                            stripes),
                                   geom_layers,
                                   intersections_matrix$layers)
  
  y_scale <- ggplot2::scale_y_discrete(
    limits = sets_limits,
    labels = function(sets) { labeller(data$non_sanitized_labels[sets]) }
  )
  user_y_scale = ComplexUpset:::get_scale(intersections_matrix, 
                                          'y')
  if (length(user_y_scale) == 0) {
    user_y_scale <- ggplot2::scale_y_discrete()
  } else {
    user_y_scale <- user_y_scale[[1]]
    user_y_scale$limits <- y_scale$limits
    user_y_scale$labels <- y_scale$labels
    y_scale <- NULL
  }
  matrix_default_colors <- list('TRUE' = 'black', 'FALSE' = 'grey85')
  matrix_guide <- "none"
  matrix_breaks <- names(matrix_default_colors)
  if (!is.null(names(stripes$colors))) {
    matrix_default_colors = c(matrix_default_colors,
                              stripes$colors
    )
    matrix_guide <- ggplot2::guide_legend()
    matrix_breaks <- names(stripes$colors)
  }
  intersections_matrix = (intersections_matrix + 
                            ggplot2::xlab(name) + 
                            scale_intersections + 
                            y_scale + 
                            ComplexUpset:::scale_if_missing(intersections_matrix,
                                                            'colour',
                                                            ggplot2::scale_color_manual(values = matrix_default_colors,
                                                                                        guide = matrix_guide,
                                                                                        breaks = matrix_breaks
                                                            )) + 
                            themes$intersections_matrix)
  rows <- list()
  if (show_overall_sizes) {
    is_set_size_on_the_right <- !is.null(set_sizes$position) && set_sizes$position == 'right'
  }
  annotation_number <- 1
  for (name in names(annotations)) {
    annotation <- annotations[[name]]
    geoms <- annotation$geom
    annotation_mode <- mode
    for (layer in annotation$layers) {
      if (inherits(layer$stat, 'StatMode')) {
        annotation_mode <- layer$stat_params$mode
      }
    }
    annotation_data <- data$with_sizes[data$with_sizes[ComplexUpset:::get_mode_presence(annotation_mode, symbol = FALSE)] == 1, ]
    if (!inherits(geoms, 'list')) {
      geoms <- list(geoms)
    }
    annotation_queries <- ComplexUpset:::intersect_queries(ComplexUpset:::queries_for(queries, 
                                                                                      name), 
                                                           data)
    if (nrow(annotation_queries) != 0) {
      highlight_data <- merge(annotation_data, 
                              annotation_queries, 
                              by.x = 'intersection', 
                              by.y = 'intersect', 
                              all.y = TRUE)
      if (is.null(annotation$highlight_geom)) {
        highlight_geom <- geoms
      } else {
        highlight_geom <- annotation$highlight_geom
        if (!inherits(highlight_geom, 'list')) {
          highlight_geom <- list(highlight_geom)
        }
      }
      geoms_plus_highlights <- ComplexUpset:::add_highlights_to_geoms(geoms, 
                                                                      highlight_geom, 
                                                                      highlight_data, 
                                                                      annotation_queries)
    } else {
      geoms_plus_highlights <- geoms
    }
    if (!is.null(annotation$top_geom)) {
      geoms_plus_highlights <- c(geoms_plus_highlights, annotation$top_geom)
    }
    if (name %in% names(themes)) {
      selected_theme <- themes[[name]]
    } else {
      selected_theme <- themes[['default']]
    }
    if (!is.null(guides) && guides == 'over' && ceiling(length(annotations) / 2) == annotation_number) {
      spacer <- patchwork::guide_area()
    } else {
      spacer <- patchwork::plot_spacer()
    }
    if (show_overall_sizes && !is_set_size_on_the_right) {
      rows[[length(rows) + 1]] <- spacer
    }
    if (ggplot2::is_ggplot(annotation)){
      if (is.null(annotation$mapping$x)){
        annotation <- annotation + 
          ggplot2::aes(x = intersection)
      }
      annotation_plot <- annotation + 
        annotation_data
      user_theme <- annotation_plot$theme
      annotation_plot <- annotation_plot + 
        selected_theme + 
        do.call(theme, user_theme)
      if (is.null(annotation_plot$default_y) && !is.null(annotation_plot$mapping$y)) {
        annotation_plot$default_y <- rlang::quo_name(annotation_plot$mapping$y)
      }
      if (is.null(annotation_plot$labels$y) || (!is.null(annotation_plot$default_y) && annotation_plot$default_y == annotation_plot$labels$y)){
        annotation_plot <- annotation_plot + ggplot2::ylab(name)
      }
    } else {
      annotation_plot <- ggplot2::ggplot(annotation_data, annotation$aes) + 
        selected_theme + 
        ggplot2::xlab(name) + 
        ggplot2::ylab(name)
    }
    user_layers <- annotation_plot$layers
    annotation_plot$layers <- c()
    annotation_plot <- annotation_plot + geoms_plus_highlights
    annotation_plot$layers <- c(annotation_plot$layers, user_layers)
    rows[[length(rows) + 1]] <- (annotation_plot + scale_intersections)
    if (show_overall_sizes && is_set_size_on_the_right) {
      rows[[length(rows) + 1]] <- spacer
    }
    annotation_number <-  annotation_number + 1
  }
  if (show_overall_sizes) {
    set_sizes_data = data$presence[data$presence$group %in% data$plot_sets_subset, ]
    if (set_sizes$filter_intersections) {
      set_sizes_data <- set_sizes_data[set_sizes_data$intersection %in% data$plot_intersections_subset, ]
    }
    overall_sizes_queries <- ComplexUpset:::set_queries(ComplexUpset:::queries_for(queries, 
                                                                                   'overall_sizes'), 
                                                        data$sanitized_labels)
    overall_sizes_highlights_data <- ComplexUpset:::get_highlights_data(set_sizes_data, 
                                                                        'group', 
                                                                        overall_sizes_queries)
    if (nrow(overall_sizes_queries) != 0) {
      highlight_geom <- set_sizes$highlight_geom
      if (!inherits(highlight_geom, 'list')) {
        highlight_geom <- list(highlight_geom)
      }
      overall_sizes_highlights_data$group <- factor(overall_sizes_highlights_data$group)
      geom <- ComplexUpset:::add_highlights_to_geoms(set_sizes$geom,
                                                     highlight_geom,
                                                     overall_sizes_highlights_data,
                                                     overall_sizes_queries,
                                                     kind = 'group')
    } else {
      geom <- set_sizes$geom
    }
    if (is_set_size_on_the_right) {
      default_scale <- ggplot2::scale_y_continuous()
    } else {
      default_scale <- ggplot2::scale_y_reverse()
    }
    set_sizes$layers <- c(ComplexUpset:::matrix_background_stripes(data, 
                                                                   stripes, 
                                                                   'vertical'),
                          geom,
                          set_sizes$layers
    )
    overall_sizes = (set_sizes + 
                       set_sizes_data + 
                       ggplot2::aes(x = group) + 
                       themes$overall_sizes + 
                       do.call(theme, set_sizes$theme) + 
                       ggplot2::coord_flip() + 
                       ggplot2::scale_x_discrete(limits = sets_limits) + 
                       ComplexUpset:::scale_if_missing(set_sizes, 
                                                       axis = 'y', 
                                                       scale = default_scale) + 
                       ComplexUpset:::scale_if_missing(set_sizes,
                                                       'colour',
                                                       ggplot2::scale_color_manual(values = matrix_default_colors,
                                                                                   guide = "none")
                       )
    )
    if (is_set_size_on_the_right) {
      matrix_row <- list(intersections_matrix, overall_sizes)
    } else {
      matrix_row <- list(overall_sizes, intersections_matrix)
    }
  } else {
    matrix_row <- list(intersections_matrix)
  }
  if (length(rows)) {
    annotations_plots <-  Reduce(f = '+', rows)
    matrix_row <- c(list(annotations_plots), matrix_row)
  } else {
    annotations_plots <- list()
  }
  plot <- Reduce(f = '+', matrix_row)
  if (show_overall_sizes) {
    if (is_set_size_on_the_right) {
      width_ratio <- 1 - width_ratio
    }
    width_ratios <- c(width_ratio, 1 - width_ratio)
  } else {
    width_ratios <- 1
  }
  if (!is.null(guides) && guides == 'over') {
    guides <- 'collect'
  }
  plot <- plot + 
    patchwork::plot_layout(widths = width_ratios,
                           ncol = 1 + ifelse(show_overall_sizes, 1, 0),
                           nrow = length(annotations) + 1,
                           heights = c(rep(1, length(annotations)), height_ratio),
                           guides = guides
    )
  if (wrap) {
    patchwork::wrap_elements(plot)
  } else {
    plot
  }
}

#' Define appearance of the stripes
#' @rdname upset_stripes
#' @param mapping additional aesthetics
#' @param geom a geom to use, should accept `x`, `y`, `xend`, `yend` and `color` aesthetics
#' @param colors a vector of colors to repeat as many times as needed for the fill of stripes, or a named vector specifying colors for values of the variable mapped to the color aesthetics in the mapping argument
#' @param data the dataset describing the sets with a column named `set` and any other columns as needed for mapping
upset_stripes <- function(mapping = ggplot2::aes(), geom = ggplot2::geom_segment(linewidth = 7), colors = c('white', 'grey95'), data = NULL) {
  stripes = list(mapping = mapping,
                 geom = geom,
                 colors = colors,
                 data = data
  )
  class(stripes) <- 'upset_stripes'
  stripes
}

#' Define upset_themes
#' @rdname upset_themes
upset_themes <- function() {
  list(intersections_matrix =
         ggplot2::theme_minimal() +
         # restauraR:::themeRestauraR() +
         ggplot2::theme(
           # hide intersections
           axis.text.x = ggplot2::element_blank(),
           axis.ticks.x = ggplot2::element_blank(),
           # hide group title
           axis.title.y = ggplot2::element_blank()
         ),
       'Intersection size' = ggplot2::theme_minimal() +
         # restauraR:::themeRestauraR() +
         ggplot2::theme(
           axis.text.x = ggplot2::element_blank(),
           axis.title.x = ggplot2::element_blank()
         ),
       overall_sizes =
         ggplot2::theme_minimal()+
         # restauraR:::themeRestauraR() +
         ggplot2::theme(
           # hide groups
           axis.title.y = ggplot2::element_blank(),
           axis.text.y = ggplot2::element_blank(),
           axis.ticks.y = ggplot2::element_blank()
         ),
       default =
         ggplot2::theme_minimal()+
         # restauraR:::themeRestauraR() +
         ggplot2::theme(
           axis.text.x = ggplot2::element_blank(),
           axis.title.x = ggplot2::element_blank()
         )
  )
}