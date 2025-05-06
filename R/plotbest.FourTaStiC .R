plotbest.FourTaStiC  <- function(group.data,
                             label.name,
                             L, E, pp = 0.08,
                             choose.plot = "all",
                             n_sam = NULL,
                             cor_list = NULL,
                             cor_sam = NULL,
                             Time = NULL,
                             E.default = TRUE,
                             alpha = NULL,
                             C = 0) {

  # choose.plot = c("sample", "cor.max", "all")
  full_data = as.data.frame(group.data)
  group_col = full_data[[label.name]]
  group_ids = sort(unique(as.numeric(as.factor(group_col))))
  max_col = ncol(full_data) - 1 - L
  mean.result = matrix(0,nrow = length(group_ids),ncol = max_col)

  get_group_data <- function(i) {
    if (choose.plot == "sample") {
      if (is.null(n_sam)) stop("Please provide 'n_sam' when choose.plot = 'sample'.")
      sampled = full_data |>
        group_by(.data[[label.name]]) |>
        slice_sample(n = n_sam) |>
        ungroup()
      each.dat = sampled |>
        filter(.data[[label.name]] == i) |>
        select(-all_of(label.name)) |>
        as.matrix()

    } else if (choose.plot == "cor.max") {
      if (is.null(cor_list) || is.null(cor_sam)) {
        stop("Please provide both 'cor_list' and 'cor_sam' when choose.plot = 'cor.max'.")
      }
      corr_matrix = cor_list[[paste0("g", i)]]
      max_row = which.max(apply(corr_matrix, 1, max, na.rm = TRUE))
      top_ids = order(corr_matrix[max_row, ], decreasing = TRUE)[1:(cor_sam - 1)]
      selected_rows = c(max_row, top_ids)

      each.dat = full_data |>
        filter(.data[[label.name]] == i) |>
        slice(selected_rows) |>
        select(-all_of(label.name)) |>
        as.matrix()

    } else {  # choose.plot == "all"
      each.dat = full_data |>
        filter(.data[[label.name]] == i) |>
        select(-all_of(label.name)) |>
        as.matrix()
      n.each = nrow(each.dat)
    }
    return(each.dat)
  }
  results = list()
  for (i in group_ids) {
    data_matrix = get_group_data(i)

    best = best.FourTaStiC(
      data = data_matrix,
      L = L,
      E = E,
      Time = Time,
      E.default = E.default,
      pp = pp,
      alpha = alpha,
      C = C
    )

    best_mat = best.matrix(data = data_matrix, best.align = best)

    # Prepare plot data
    shift_data = melt(t(best_mat$shift.mat[, 1:max_col]))
    shift_trend_data = melt(t(best_mat$shift_tilt.mat[, 1:max_col]))

    colnames(shift_data) <- colnames(shift_trend_data) <- c("Time", "ID", "Value")

    plot_time = ggplot(shift_data, aes(x = Time, y = Value, group = ID)) +
      geom_line() +
      geom_point()

    plot_timetrend = ggplot(shift_trend_data, aes(x = Time, y = Value, group = ID)) +
      geom_line() +
      geom_point()

    results[[paste0("group_", i)]] = list(
      best_result = best,
      best_shift.data = best_mat$shift.mat,
      best_shift_trend_data = best_mat$shift_tilt.mat,
      plot_time = plot_time,
      plot_timetrend = plot_timetrend
    )
    # overall mean
    mean.result[i,] = best_mat$shift.mat[,1:max_col] |> colMeans()
  }
  mean_plot_dat = melt(t(mean.result))
  colnames(mean_plot_dat) = c("Time","Group","Mean")
  plot.mean = mean_plot_dat |>
    ggplot(aes(x = Time,y = Mean,group=as.factor(Group),colour = as.factor(Group))) +
    geom_line() +
    geom_point() +
    labs(color = "Group") +
    scale_color_paletteer_d("ggthemr::flat") +
    scale_x_continuous(breaks = pretty_breaks()) +
    theme_bw()
  results[["Overall_mean"]] =  plot.mean
  return(results)
}
