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

#' Multivariable Overview Plot colored by source
#'
#' @param df data frame
#' @param colorby char. str. of variable name to which the color should be applied
#' @param ignore.idx if TRUE the index variable will be ignored
#' @param cont.vars str. of continuous variable's names.
#' @param disc.cont.ratio relative number of rows of discrete and continuous plots.
#'
#' @return ggplot2
#' @export
overview_plt_max_grouped <- function(df, colorby="study_source", ignore.idx=T, cont.vars=c("age_diag", "IAsize_diag", "IAsize_diag_log"), disc.cont.ratio=c(4,1)){
  paint <- c('#b4d2b1', '#568f8b', '#1d4a60', '#cd7e59', '#ddb247', '#d15252')

  if (ignore.idx){
    df_temp <- df %>%
      select(-contains("ID"))
  } else {
    df_temp <- df
  }

  # separate continuous and discrete variables
  df_temp_disc <- df_temp %>%
    select(-cont.vars)

  df_temp_cont <- df_temp %>%
    select(c(cont.vars, "study_source"))

  # Plot discrete values
  if (!is.null(colorby)){
    df_temp_disc <- df_temp_disc  %>%
      pivot_longer(!study_source, names_to = "key", values_to = "value")
  } else {
    df_temp_disc <- df_temp_disc  %>%
      pivot_longer(names_to = "key", values_to = "value")
  }

  plt_disc <- df_temp_disc %>%
    ggplot(., aes(x = value, fill=study_source)) +
    geom_histogram(stat = "count", position = "dodge") +
    facet_wrap(.~key,
               nrow = 5,
               scales = "free",
               drop = FALSE)+
    aes(stringr::str_wrap(value, 2))+
    scale_x_discrete(labels = function(x) stringr::str_trunc(x, 8))+
    # scale_x_discrete(labels = function(x) abbreviate(x, minlength=5))+
    theme_minimal()+
    scale_fill_manual(values = paint) +
    ggtitle(deparse(substitute(df)))+
    theme(
      legend.spacing = unit(1, "cm"),
      # axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,margin = margin(10,10,10,10)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # coord_flip()

  # Plot continuous variables
  if (!is.null(colorby)){
    df_temp_cont <- df_temp_cont  %>%
      pivot_longer(!study_source, names_to = "key", values_to = "value")
  } else {
    df_temp_cont <- df_temp_cont  %>%
      pivot_longer(names_to = "key", values_to = "value")
  }

  plt_cont <- df_temp_cont %>%
    ggplot(., aes(x = value, fill=study_source)) +
    geom_boxplot(position = "dodge") +
    facet_wrap(.~key,
               scales = "free",
               drop = FALSE)+
    theme_minimal()+
    scale_fill_manual(values = paint) +
    ggtitle(NULL)+
    ylab(NULL)+
    theme(
      legend.spacing = unit(1, "cm"),
      legend.position = "none",
      # axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,margin = margin(10,10,10,10)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_flip()

  # combine disc. and cont. plots
  plt <- patchwork::wrap_plots(plt_disc, plt_cont, ncol=1, heights = disc.cont.ratio)
  # plt <- gridExtra::grid.arrange(plt_disc, plt_cont, ncol=1)
  return(plt)
  # return(list(plt_disc, plt_cont))
}

#' Multivariable Overview Plot colored by IA rupture
#'
#' @param df data frame
#' @param colorby DEPRECATED char. str. of variable name to which the color should be applied
#' @param ignore.idx if TRUE the index variable will be ignored
#' @param cont.vars str. of continuous variable's names.
#' @param disc.cont.ratio relative number of rows of discrete and continuous plots.
#'
#' @return ggplot2
#' @export
overview_plt_max_grouped_rupture <- function(df, colorby="IAruptured", ignore.idx=T, cont.vars=c("age_diag", "IAsize_diag", "IAsize_diag_log"), disc.cont.ratio=c(4,1)){
  paint <- c('#b4d2b1', '#568f8b', '#1d4a60', '#cd7e59', '#ddb247', '#d15252')

  if (ignore.idx){
    df_temp <- df %>%
      select(-contains("ID"))
  } else {
    df_temp <- df
  }

  # separate continuous and discrete variables
  df_temp_disc <- df_temp %>%
    select(-cont.vars)

  df_temp_cont <- df_temp %>%
    select(c(cont.vars, "IAruptured"))

  # Plot discrete values
  if (!is.null(colorby)){
    df_temp_disc <- df_temp_disc  %>%
      pivot_longer(!IAruptured, names_to = "key", values_to = "value")
  } else {
    df_temp_disc <- df_temp_disc  %>%
      pivot_longer(names_to = "key", values_to = "value")
  }

  plt_disc <- df_temp_disc %>%
    ggplot(., aes(x = value, fill=IAruptured)) +
    geom_histogram(stat = "count", position = "dodge") +
    facet_wrap(.~key,
               nrow = 5,
               scales = "free",
               drop = FALSE)+
    aes(stringr::str_wrap(value, 2))+
    scale_x_discrete(labels = function(x) stringr::str_trunc(x, 8))+
    # scale_x_discrete(labels = function(x) abbreviate(x, minlength=5))+
    theme_minimal()+
    scale_fill_manual(values = paint) +
    ggtitle(deparse(substitute(df)))+
    theme(
      legend.spacing = unit(1, "cm"),
      # axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,margin = margin(10,10,10,10)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  # coord_flip()

  # Plot continuous variables
  if (!is.null(colorby)){
    df_temp_cont <- df_temp_cont  %>%
      pivot_longer(!IAruptured, names_to = "key", values_to = "value")
  } else {
    df_temp_cont <- df_temp_cont  %>%
      pivot_longer(names_to = "key", values_to = "value")
  }

  plt_cont <- df_temp_cont %>%
    ggplot(., aes(x = value, fill=IAruptured)) +
    geom_boxplot(position = "dodge") +
    facet_wrap(.~key,
               scales = "free",
               drop = FALSE)+
    theme_minimal()+
    scale_fill_manual(values = paint) +
    ggtitle(NULL)+
    ylab(NULL)+
    theme(
      legend.spacing = unit(1, "cm"),
      legend.position = "none",
      # axis.text.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1,margin = margin(10,10,10,10)),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_flip()

  # combine disc. and cont. plots
  plt <- patchwork::wrap_plots(plt_disc, plt_cont, ncol=1, heights = disc.cont.ratio)
  # plt <- gridExtra::grid.arrange(plt_disc, plt_cont, ncol=1)
  return(plt)
  # return(list(plt_disc, plt_cont))
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
#' \dontrun{
#' printorsave_dfsummary(df=dfucl, FILENAME = "dfucl", SAVE = FALSE, PLOTPATH = PLOTPATH)
#' }
printorsave_dfsummary <- function(df, FILENAME, SAVE=SAVE, PLOTPATH=PLOTPATH){
  if(SAVE){
    summarytools::view(summarytools::dfSummary(df), method = "browser", file = paste0(PLOTPATH, "/dfsummary_", FILENAME, ".html"))
  } else {
    summarytools::dfSummary(df)
  }
}
