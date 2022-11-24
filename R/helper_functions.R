med_iqr <- function(x){
  
  med_x <- round(median(x, na.rm = T), 1)
  iqr_x <- round(quantile(x, probs = c(0.25, 0.75), na.rm = T), 1)
  
  res <- "-"
  
  if(!is.na(med_x)){
    res <- paste0(med_x, " (", iqr_x[1], "-", iqr_x[2],")")
  }
  
  return(res)
  
}

mean_std <- function(x){
  
  mean_x <- round(mean(x, na.rm = T), 2)
  sd_x <- round(sd(x, na.rm = T), 2)
  
  res <- "-"
  
  if(!is.na(mean_x)){
    res <- paste0(mean_x, " (±", sd_x,")")
  }
  
  return(res)
  
}

number_percent <- function(df, gr1, gr2){
  
  df %>% group_by(!!sym(gr1), !!sym(gr2)) %>%
    summarise(n = n()) %>%
    mutate(perc = round(n/sum(n) * 100, 2)) %>%
    mutate(comb =  paste0(n, " (", perc,"%)")) %>%
    tidyr::pivot_wider(id_cols = c(gr1, gr2), names_from = gr1, values_from = "comb") %>%
    mutate(across(.cols = c(-!!sym(gr2)),.fns = function(x) replace(x, which(is.na(x)), "0 (0.00%)")))
  
}

number_percent_one_factor <- function(df, gr1){
  
  df %>% group_by(!!sym(gr1)) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(perc = round(n/sum(n) * 100, 2)) %>%
    mutate(comb =  paste0(n, " (", perc,"%)")) %>%
    tidyr::pivot_wider(id_cols = c(gr1), names_from = gr1, values_from = "comb") #%>%
  # mutate(across(.cols = c(-!!sym(gr1)),.fns = function(x) replace(x, which(is.na(x)), "0 (0.00%)")))
  
}

mut_num_perc <- function(x, char_sel){
  
  abs <- sum(x %in% char_sel, na.rm = T)
  rel <- round(abs/length(x)* 100, digits = 2)
  
  return(paste0(abs, " (", rel, "%)"))
}

fisher_test_table <- function(df, gr_row, gr_col){
  
  ft_table <- df %>% group_by(!!sym(gr_row), !!sym(gr_col)) %>% count(name = "Number of Subjects") %>% 
    tidyr::pivot_wider(id_cols = c(gr_col, "Number of Subjects"), names_from = gr_row, values_from = "Number of Subjects") 
  
  ft_res <- fisher.test(ft_table[,-1])
  
  kable_table <- df %>% group_by(!!sym(gr_row), !!sym(gr_col)) %>% count(name = "n") %>% 
    group_by(!!sym(gr_col)) %>% mutate(val = paste0(n, " (", round(n/sum(n) * 100, 2), "%)" )) %>%
    tidyr::pivot_wider(id_cols = c(gr_row, "val", gr_col), names_from = gr_col, values_from = "val")
  
  knitr::kable(cbind(kable_table, "Odds-Ratio" = round(ft_res$estimate, 2), "P-value" = round(ft_res$p.value, 2)), align = "c") %>%
    kableExtra::kable_styling(full_width = F, position = "left")%>%
    kableExtra::column_spec(1, bold = T) %>%
    kableExtra::collapse_rows(columns = 3:5, valign = "middle")
}

chisq_test_table <- function(df, gr_row, gr_col){
  
  chi_table <- df %>% group_by(!!sym(gr_row), !!sym(gr_col)) %>% count(name = "Number of Subjects") %>% 
    tidyr::pivot_wider(id_cols = c(gr_row, "Number of Subjects"), names_from = gr_col, values_from = "Number of Subjects")
  
  chisq_res <- chisq.test(chi_table[,-1])
  
  kable_table <- df %>% group_by(!!sym(gr_row), !!sym(gr_col)) %>% count(name = "n") %>% 
    group_by(!!sym(gr_col)) %>% mutate(val = paste0(n, " (", round(n/sum(n) * 100, 2), "%)" )) %>%
    tidyr::pivot_wider(id_cols = c(gr_row, "val", gr_col), names_from = gr_col, values_from = "val")
  
  knitr::kable(cbind(kable_table, "Odds-Ratio" =  "-", "P-value" = round(chisq_res$p.value, 2)), align = "c") %>%
    kableExtra::kable_styling(full_width = F, position = "left")%>%
    kableExtra::column_spec(1, bold = T) %>%
    kableExtra::collapse_rows(columns = dim(kable_table)[2]:(dim(kable_table)[2] + 2), valign = "middle")
}

model_specifics <- function(model, data){
  
  prds <- model %>% predict(data) 
  rmse <- caret::RMSE(prds, data$logIgG)
  r2 <- caret::R2(prds, data$logIgG)
  
  list(RMSE = rmse, R2 = r2)
  
}

fisher_plot_odds <- function(fisher_test_df, ylab_add = "Coadministration", wrap = F, ylim = NULL){
  
  fisher_plt_df <- fisher_test_df %>%
    select(group2, estimate, matches("conf|super"), p.value) %>%
    unique() %>% mutate(p.adj = p.adjust(p.value, "BY"))
  
  odds_plot <- ggplot2::ggplot(data = fisher_plt_df, ggplot2::aes(x = group2, y = estimate))
  
  if(wrap){
    odds_plot <- odds_plot + ggplot2::facet_wrap(~super_group, nrow = 2)
  }
  odds_plot <- odds_plot + 
    ggplot2::geom_point(cex = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.5,
                           position = ggplot2::position_dodge2(), show.legend = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::scale_x_discrete(labels = c("Local reactions", "Headache", "Muscle Pain", "Fever and/or Chills", "Fatigue", "Other s.e.")) +
    ggsci::scale_color_nejm() +
    ggplot2::theme(axis.text = ggplot2::element_text(size = 12, colour = "black"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title = ggplot2::element_text(face = "bold", size = 14),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 12),
                   legend.position = "bottom",
                   strip.text = ggplot2::element_text(size = 14, face = "bold"), 
                   strip.background = ggplot2::element_rect(fill = "white"))+
    ggplot2::ylab(paste0("OR ", ylab_add)) +
    ggplot2::xlab("side effects")
  
  if(!is.null(ylim)){
    
    odds_plot <- odds_plot + ggplot2::ylim(ylim)
  }
  
  if(dim( fisher_plt_df %>% filter(p.adj < 0.05))[1] > 0){
    odds_plot <- odds_plot +  ggplot2::geom_text(data = fisher_plt_df %>% filter(p.adj < 0.05), ggplot2::aes(x = group2, y = conf.high, label = paste0("italic(p)== ",round(p.adj, 2))), vjust = -0.5, parse = T)
  }
  
  odds_plot
}

barplot_sideeffects_percentage <- function(p.adj_fisher_tests_df, wrap = F){
  
  plt_df <- c()
  
  if(wrap){
    plt_df <- p.adj_fisher_tests_df %>% group_by(super_group, group1, group2) %>% 
      mutate(n =  gr2.1 + gr2.2,
             percentage = round(gr2.1/n * 100, 2)) %>% 
      select(group1, group2, percentage, p.adjusted)
  }else{
    
    plt_df <- p.adj_fisher_tests_df %>% group_by(group1, group2) %>% 
      mutate(n =  gr2.1 + gr2.2,
             percentage = round(gr2.1/n * 100, 2)) %>% 
      select(group1, group2, percentage, p.adjusted)
  }
  
  p_val_data <- plt_df %>% mutate(group1 = plt_df$group1[1]) %>% 
    select(group2, p.adjusted, percentage)  %>% 
    mutate(percentage = max(percentage)) %>% 
    unique() %>% ungroup()
  
  bar_plt <- ggplot2::ggplot(data = plt_df, ggplot2::aes(x = group2, y = percentage, fill = group1))
  
  if(wrap){
    
    bar_plt <- bar_plt + ggplot2::facet_wrap(~super_group, nrow = 2)
  }
  
  bar_plt <- bar_plt + 
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge2()) +
    ggplot2::xlab("Side effects") +
    ggplot2::ylab("% participants") +
    ggplot2::theme_bw() +
    ggsci::scale_fill_nejm() +
    ggplot2::scale_x_discrete(labels = c("Local reactions",
                                         "Headache", "Muscle Pain", "Fever and/or Chills", "Fatigue", "Other")) +
    ggplot2::scale_y_continuous(expand = c(0,1), limits = c(0,100)) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = "Effect")) +
    ggplot2::theme(axis.title = ggplot2::element_text(color = "black", size = 13, face = "bold"),
                   axis.text = ggplot2::element_text(color = "black", size = 12),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                   strip.text = ggplot2::element_text(size = 14, face = "bold"), 
                   strip.background = ggplot2::element_rect(fill = "white"),
                   legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 10),
                   axis.title.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.justification = c("left", "top"),
                   legend.box.just = "left",
                   legend.margin = ggplot2::margin(6, 6, 6, 6),
                   legend.background = ggplot2::element_rect(fill = NA))+
    ggplot2::geom_text(data = p_val_data,
                       mapping = ggplot2::aes(x = group2, y = percentage + 1, label = paste0("italic(p)== ", round(p.adjusted, digits = 3))),
                       hjust   = 0.5, vjust = 0, size = 4, parse = T)
  
  bar_plt
}