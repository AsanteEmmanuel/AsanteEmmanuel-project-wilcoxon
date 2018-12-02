
#' Function to generate a table to Conduct Wilcoxon Test on each row.
#'
#' @param table_length length of table you want to generate or the number of sample pairs you want to run the hypothesis test on.
#' @param sample_from Distribution to sample from e.g. rpois
#' @param ... Arguments that may be passed to the sample_from distribution
#' @param alpha Significance level
#'
#' @return returns the rejection rate of the entire table. i.e. how many of the hypothesis were rejected when compared to alpha
#' @export
#'
#' @examples wilcox_one_table(table_length = 1000, sample_from = rpois, alpha = 0.05, lambda = 20)
wilcox_one_table <- function(table_length = 1000, sample_from = rpois, ..., alpha = 0.05){
  wilcox_table <- data_frame(index = 1:table_length)
  wilcox_table <- wilcox_table %>% 
    mutate(sample_1 = rerun(table_length, sample_from(30 + rbinom(1, size = 20, prob = 0.5), ...)),
           sample_2 = rerun(table_length, sample_from(30 + rbinom(1, size = 20, prob = 0.5), ...)),
           wilcox_pvalue = flatten_dbl(map2(sample_1, sample_2, ~wilcox.test(.x, .y, exact = FALSE, conf.level = 1 - alpha)$p.value)), 
           Reject = ifelse(wilcox_pvalue <= alpha, TRUE, FALSE))
  #output <- list(type_1_error_rate = mean(wilcox_table$Reject), one_table = wilcox_table)
  output <- mean(wilcox_table$Reject)
  # Invisible function prevents the output from showing
  invisible(output)
}
