#' Single Variabel Overview Plot
#'
#' @param df data.frame()
#' @param x str. Columnname.
#' @param decreasing is set to FALSE, columns are not sorted. If TRUE they are arranged in
#' decreasing order.
#' @param warnings prints warnings if TRUE, FALSE suppresses warnings.
#'
#' @return ggplot object
#'
#' @importFrom dplyr group_by summarise mutate arrange desc n
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @import ggplot2
#' @export
overview_plt <- function(df, x, decreasing = FALSE, warnings = FALSE){
  if (decreasing) {
    plt <- gather(df, cols, value) %>%
      subset(cols == x) %>%
      group_by(value) %>%
      summarise(cnt=n(), .groups = "keep") %>%
      mutate(cnt = factor(cnt)) %>%
      mutate(value = factor(value)) %>%
      # mutate(value = fct_reorder(value, desc(cnt), .desc = TRUE)) %>%
      arrange(desc(cnt)) %>%
      mutate(cnt = factor(cnt)) %>%

      ggplot(aes(x=reorder(value, desc(cnt)), y=cnt)) +
      geom_col(show.legend = TRUE) +
      # stat_bin(aes(x = recruBasis),  geom="text", vjust=-.5)+
      # geom_text(aes(label=stat(count)), stat='count', nudge_y=30)+
      theme_minimal()+
      ggtitle(paste(x))+
      theme(
        title = element_text(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    print(plt)
  } else {
    plt <- ggplot(df, aes_string(x=x)) +
      geom_histogram(stat = "count", show.legend = TRUE) +
      # stat_bin(aes(x = recruBasis),  geom="text", vjust=-.5)+
      geom_text(aes(label=stat(count)), stat='count', nudge_y=30)+
      theme_minimal()+
      ggtitle(paste(x))+
      theme(
        title = element_text(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )

    if (warnings){
      print(plt)
    } else {
      suppressWarnings(print(plt))
    }
  }
}

#' Multivariable Overview Plot
#'
#' @param df data.frame()
#'
#' @return ggplot object
#'
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @import ggplot2
#' @export
overview_plt_max <- function(df){
  plt <- df %>%
    gather(cols, value) %>%
    ggplot(., aes(x = value)) +
    geom_histogram(stat = "count") +
    facet_wrap(.~cols,
               nrow = 5,
               scales = "free",
               drop = FALSE)+
    aes(stringr::str_wrap(value, 2))+
    scale_x_discrete(labels = function(x) stringr::str_trunc(x, 8))+
    # scale_x_discrete(labels = function(x) abbreviate(x, minlength=5))+
    theme_minimal()+
    ggtitle(deparse(substitute(df)))+
    theme(
      legend.spacing = unit(1, "cm"),
      # axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,margin = margin(10,10,10,10)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )+
    coord_flip()
  print(plt)
}

#' Save or print summarytools::dfSummary()
#'
#' Prints summary of `df` or saves a html document named "dfsummary_<FILENAME>" at
#' PLOTPATH.
#'
#' @param df data.frame.
#' @param FILENAME str. with file name suffix: "dfsummary_<FILENAME>"
#' @param SAVE TRUE if output should be stored as html file or printed to the console.
#' @param PLOTPATH str. with path to save. i.e. `/home/user/Documents`
#'
#' @return Data Frame Summary or NULL
#' @export
#'
#' @examples
#' \donotrun{
#' printorsave_dfsummary(df=dfucl, FILENAME = "dfucl", SAVE = FALSE, PLOTPATH = PLOTPATH)
#' }
printorsave_dfsummary <- function(df, FILENAME, SAVE=SAVE, PLOTPATH=PLOTPATH){
  if(SAVE){
    summarytools::view(summarytools::dfSummary(df), method = "browser", file = paste0(PLOTPATH, "/dfsummary_", FILENAME, ".html"))
  } else {
    summarytools::dfSummary(df)
  }
}
