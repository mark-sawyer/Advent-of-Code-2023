
if (!(exists("setup_has_run") && setup_has_run)) {
  before_setup_environment <- ls()  # Assumes nothing from Setup.R exists yet.
  
  # Set options ----
  knitr::opts_chunk$set(echo=FALSE)
  knitr::opts_chunk$set(warning=FALSE)
  knitr::opts_chunk$set(message=FALSE)
  knitr::opts_chunk$set(comment=NA)
  options(dplyr.summarise.inform=FALSE)
  
  # Core Libraries ----
  library(tidyverse)
  library(gridExtra)
  library(knitr)
  library(kableExtra)
  
  # Colour palette ----
  sp_col_100 <- c(
    rgb(0, 46, 93, max=255),
    rgb(0, 178, 169, max=255),
    rgb(132, 189, 0, max=255),
    rgb(225, 205, 0, max=255),
    rgb(166, 166, 166, max=255),
    rgb(79, 117, 139, max=255),
    rgb(0, 0, 0, max=255),
    rgb(115, 24, 44, max=255)
  )
  
  # Set the defaults for ggplot ----
  assign("scale_colour_discrete", function(..., values = sp_col_100) scale_colour_manual(..., values = values), globalenv())
  
  assign("scale_fill_discrete", function(..., values = sp_col_100) scale_fill_manual(..., values = values), globalenv())
  
  theme_set({
    theme_classic(base_size=11, base_family="") %+replace%
      theme(
        legend.position = "bottom",
        axis.line = element_line(color="#bbbbbb"),
        axis.ticks= element_line(color="#bbbbbb")
      )
  })
  
  # My standard functions ----
  {
    # Handy ----
    `%nin%` <- function(x, y) { !(x %in% y) }
    
    clamp <- function(x, min, max) {
      if_else(x < min, min, x) %>%
        { if_else(. > max, max, .) }
    }
    
    difference_within <- function(x, y, thresh=0.000001) {
      abs(x - y) < thresh
    }
    
    inv_cloglog <- function(x) {
      1 - exp(-exp(x))
    }
    
    lerp <- function(prop, val1, val2) {
      (1 - prop)*val1 + prop*val2
    }
    
    logit <- function(x) {
      exp(x) / (1 + exp(x))
    }
    
    logodds <- function(x) {
      log(x / (1 - x))
    }
    
    percent_char <- function(v, digits=1L) {
      sprintf(str_c("%.", digits, "f"), v * 100) %>% str_c("%")
    }
    
    rgb_to_hex <- function(r, g, b) {
      if (missing(g) & missing(b)) {
        g <- r
        b <- r
      }
      val_to_hex <- function(val) {
        sixteens <- c(0:9, LETTERS[1:6])[(val / 16) %>% floor() %>% { . + 1 }]
        ones <- c(0:9, LETTERS[1:6])[(val %% 16) %>% { . + 1 }]
        str_c(sixteens, ones)
      }
      str_c(
        "#",
        val_to_hex(r),
        val_to_hex(g),
        val_to_hex(b)
      )
    }
    
    rm_starting_with <- function(pattern) {
      pattern_length <- str_length(pattern)
      rm(
        list=ls(name=.GlobalEnv)[str_sub(ls(name=.GlobalEnv), 1, pattern_length) == pattern],
        envir=.GlobalEnv
      )
    }
    
    round_to_nearest <- function(d, nearest=1) {
      (d / nearest) %>% round() %>% { . * nearest }
    }
    
    runfunc <- function(d, f) {
      if (missing(f)) d() else f(d)
    }
    
    standardise <- function(x, zero_one=FALSE) {
      if (zero_one) {
        max <- max(x)
        min <- min(x)
        range <- max - min
        (x - min) / range
      }
      else (x - mean(x)) / sd(x)
    }
    
    # Shortcuts ----
    convert_grade_type <- function(x) {
      if (is.numeric(x)) recode(x, `1`="E", `2`="D", `3`="C", `4`="B", `5`="A")
      else if (is.character(x)) recode(x, A=5L, B=4L, C=3L, D=2L, E=1L)
    }
    
    install_packages <- function(s) {
      install.packages(s, repos=NULL, type="source")
    }
    
    my_theme <- function(...) {
      theme(
        panel.background=element_rect(fill=NA, colour="grey"),
        strip.background=element_rect(fill=NA, colour="grey"),
        ...
      )
    }
    
    nest_all <- function(d, col_name="data") {
      d %>%
        nest_by(nest=NA_real_, col_name=col_name) %>%
        select(-nest)
    }
    
    nest_by <- function(d, ..., col_name="data") {
      d %>%
        group_by(...) %>%
        nest() %>%
        ungroup() %>%
        {
          x <- .
          names(x)[names(x) == "data"] <- col_name
          x
        }
    }
    
    pullfirst <- function(d, ...) {
      d %>%
        pull(...) %>%
        first()
    }
    
    rotate_text <- function(...) {
      element_text(angle=90, vjust=0.5, hjust=1, ...)
    }
    
    scale_x_breaks <- function(...) {
      scale_x_continuous(breaks=scales::pretty_breaks(), ...)
    }
    
    scale_x_off <- function(...) {
      scale_x_continuous(breaks=NULL, ...)
    }
    
    scale_x_percent <- function(...) {
      scale_x_continuous(breaks=scales::pretty_breaks(), labels=scales::percent_format(), ...)
    }
    
    scale_y_breaks <- function(...) {
      scale_y_continuous(breaks=scales::pretty_breaks(), ...)
    }
    
    scale_y_off <- function(...) {
      scale_y_continuous(breaks=NULL, ...)
    }
    
    scale_y_percent <- function(...) {
      scale_y_continuous(breaks=scales::pretty_breaks(), labels=scales::percent_format(), ...)
    }
    
    set_seed <- function(d, n=1) {
      set.seed(n)
      d
    }
    
    # Misc ----
    avg_z_in_range <- function(plow, phigh) {
      plow <- if_else(plow < 0, 0, plow) %>% { if_else(. >= 1, 1 - .Machine$double.eps, .) }
      phigh <- if_else(phigh <= 0, .Machine$double.eps, phigh) %>% { if_else(. > 1, 1, .) }
      
      prob_range <- phigh - plow
      zlow <- qnorm(plow)
      zhigh <- qnorm(phigh)
      low <- if_else(is.infinite(zlow), 0, exp(-((zlow^2) / 2)))
      high <- if_else(is.infinite(zhigh), 0, exp(-((zhigh^2) / 2)))
      integral <- -(1 / (sqrt(2*pi))) * (high - low)
      if_else(
        plow == phigh,
        qnorm(phigh),
        integral / prob_range
      )
    }
    
    create_slickr <- function(w, h, loop_var, plot_func, ...) {
      # Create plots ----
      plot_folder <- str_c(getwd(), "/slickR plots")
      if (!file.exists(plot_folder)) stop("'slickR plots' folder does not exist.")
      for (i in 1:length(loop_var)) {
        temp <- plot_func(loop_var[i], ...)
        ggsave(
          filename=str_c(plot_folder, "/", str_pad(i, 3L, "left", "0"), ".png"),
          width=w,
          height=h
        )
      }
      
      # Create slickR ----
      slickR_obj <- slickR(
        obj=list.files(plot_folder, full.names=TRUE),
        width=w * 67.5,
        height=h * 67.5
      ) +
        settings(dots=TRUE)
      
      # Delete plot files ----
      files <- list.files(plot_folder)
      unlink(str_c(plot_folder, "/", files))
      
      # Return ----
      slickR_obj
    }
    
    get_iterated_values <- function(iter_func, ..., iterations=100L, print=TRUE) {
      iter_vals <- numeric(iterations)
      
      for (i in 1:iterations) {
        if (print) print(i)
        iter_vals[i] <- iter_func(...)
      }
      
      iter_vals
    }
    
    make_gif <- function(plot_func, frames, folder, gif_name, fps=10, delete_images=TRUE, ...) {
      for (i in 1:frames) {
        p <- plot_func(i, ...)
        
        ggsave(
          filename=str_c("image_", str_pad(i, 3L, "left", "0"), ".png"),
          plot=p,
          path=folder
        )
      }
      
      magick::image_write(
        image=magick::image_animate(
          magick::image_join(map(
            str_c(folder, "/image_", str_pad(1:frames, 3L, "left", "0"), ".png"),
            magick::image_read
          )),
          fps=fps
        ),
        path=str_c(folder, "/", gif_name, ".gif")
      )
      
      if (delete_images) {
        files <- list.files(folder)
        image_files <- files[which(str_detect(files, "image_"))]
        unlink(str_c(folder, "/", image_files))
      }
    }
    
    pack_rows_for_kable <- function(kable_obj, pack_var) {
      # This function will apply pack_rows() for each group of a variable.
      # It expects the groups to be ordered together within pack_var.
      
      temp <- tibble(pack_row_var=pack_var) %>%
        mutate(group=row_number(), .by=pack_row_var) %>%
        mutate(
          group_offset=c(group[2:n()], 1L),
          is_start=group == 1L,
          is_end=group_offset == 1L
        )
      
      starts <- which(temp$is_start)
      ends <- which(temp$is_end)
      
      for (i in 1:length(starts)) {
        kable_obj <- kable_obj %>%
          pack_rows(
            group_label=as.character(temp$pack_row_var[starts[i]]),
            start_row=starts[i],
            end_row=ends[i],
            background=c_kable_grey
          )
      }
      
      kable_obj
    }
    
    print_count <- function() {
      print(x_count)
      x_count <<- x_count + 1L
    }
    
    rm_non_setup <- function() {
      rm(
        list=ls(name=.GlobalEnv)[ls(name=.GlobalEnv) %nin% c(setup_environment, "setup_environment")],
        envir=.GlobalEnv
      )
    }
    
    set_up_count <- function(d) {
      x_count <<- 1L
      d
    }
    
    time_code <- function(code) {
      start <- lubridate::now()
      eval(code)
      end <- lubridate::now()
      end - start
    }
    
    # Advent ----
    get_input <- function(day, practice=TRUE, name) {
      if (missing(name)) {
        address <- str_c(
          "Days/Day ",
          day,
          if_else(practice, "/practice", "/input"),
          ".txt"
        )
      }
      else {
        address <- str_c(
          "Days/Day ",
          day,
          "/",
          name,
          ".txt"
        )
      }
      
      readLines(address)
    }
    
    turn_into_matrix <- function(tib) {
      names(tib) <- "m"
      tib <- tib %>%
        mutate(
          m=str_split(m, ""),
          row=1:n(),
          col=map(m, ~ 1:length(.x))
        ) %>%
        unnest(c(m, col)) %>%
        pivot_wider(names_from=col, values_from=m) %>%
        select(-row) %>%
        as.matrix()
      colnames(tib) <- NULL
      tib
    }
  }
  
  # Useful constants ----
  c_kable_grey <- rgb_to_hex(249)
  c_kable_vline <- "1px solid gainsboro"
  c_sp_gradient <- sp_col_100[c(5, 6, 1:4)]
  
  # Finish ----
  setup_has_run <- TRUE
  setup_environment <- ls()[ls() %nin% before_setup_environment]
  rm(before_setup_environment)
}
