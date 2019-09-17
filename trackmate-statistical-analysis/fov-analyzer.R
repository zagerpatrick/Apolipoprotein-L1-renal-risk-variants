####### OBJECTIVE #######
## Given an output file from TrackMate:
##  - Calculate Fluorescence (F) Deltas per cell & normalize F intensity data
##  - Apply xyz threshold to tag a positive cell/event

#### Library Setup ####
libs <- c("rio", "readxl", "dplyr", "reshape2", "ggplot2", "glue",
          "rstudioapi", "tibble")
missing_libs <- setdiff(libs, rownames(installed.packages()))

if(length(missing_libs) > 0) {
    message("=====> Installing required R packages <=====")
    install.packages(missing_libs)
}

loaded <- suppressPackageStartupMessages(sapply(libs, library,
                                                character.only = TRUE))
theme_set(theme_bw())

#### Input Setup ####
input_file <- selectFile("Select TrackMate analysis file:")
if(is.null(input_file)) {
    stop("No input file found.")
}

tabs <- excel_sheets(input_file) %>% grep("raw data", ., value = TRUE)
if(length(tabs) == 0) {
    stop("No worksheets with 'raw data' found in: ", input_file)
}

message("=====> Loading Data <=====")
all_dat <- import_list(input_file, rbind = TRUE, which = tabs,
                       rbind_label = "Tab_Group")
all_dat <- all_dat %>% select(-X__1)
names(all_dat) <- gsub(" +", "_", names(all_dat))
all_dat <- all_dat %>% rename(Cell = Track_ID)

#### Analysis Setup ####
f0_frames <- 1:3
F_Norm_cutoff <- 3.33
num_in_row <- 2

#### Analysis ####
message("=====> Analyzing Data <=====")
all_dat <- all_dat %>% arrange(Tab_Group, Cell, Frame) %>%
    filter(!is.na(Mean_intensity) & !is.na(Cell)) %>%
    group_by(Tab_Group, Cell) %>%
    mutate(F0 = mean(Mean_intensity[seq_along(Frame) %in% f0_frames]),
           F_Norm = (Mean_intensity - F0)/F0,
           Is_Pos = F_Norm >= F_Norm_cutoff) %>%
    ungroup

analyzed <- all_dat %>%
    group_by(Tab_Group, Cell) %>%
    summarise(Labels = glue("ID{min(ID)} -> ID{max(ID)}"),
              Frames = n_distinct(Frame),
              Positive_Frames = sum(Is_Pos),
              glm_intercept = coef(glm(Frame ~ F_Norm))[1],
              glm_coef = coef(glm(Frame ~ F_Norm))[2],
              Is_Positive = any(rle(Is_Pos)$lengths >= num_in_row &
                                    rle(Is_Pos)$values == TRUE) &
                  (glm_intercept > 1 | glm_coef > 0))

len_uniq <- function(x) length(unique(x))

analyzed_sum <- analyzed %>% group_by(Tab_Group) %>%
    summarise(Total_Cells = len_uniq(Cell),
              TRUE_Cells = len_uniq(Cell[Is_Positive]),
              FALSE_Cells = len_uniq(Cell[!Is_Positive]),
              Percent_TRUE = round(100 * (TRUE_Cells / Total_Cells), 2))

bore <- cbind(Tab_Group = "Grand Total",
              as.data.frame(t(colSums(analyzed_sum[-1]))))
analyzed_sum <- bind_rows(analyzed_sum, bore)
rm(bore)

#### Output ####
message("=====> Ouputting Analysis <=====")

output_file <- sub("(.+)\\.(.+)", "\\1_analyzed.\\2", input_file)
export(list("raw_data" = all_dat, "analyzed" = analyzed, "summary" = analyzed_sum),
       output_file)

for(f in unique(all_dat$Tab_Group)) {
    message("... making plot for tab: ", f)

    p <- left_join(all_dat, analyzed) %>% filter(Tab_Group == f) %>%
        ggplot(data = ., aes(x = Frame, y = F_Norm, colour = Is_Positive)) +
        geom_point(size = rel(1), shape = 1) +
        facet_wrap(~Cell, labeller = "label_both", scales = "free_y") +
        geom_hline(yintercept = F_Norm_cutoff, color = "red")

    ggsave(p, file = file.path(dirname(input_file), glue("{f}_plot.pdf")),
           width = 17, height = 11)
}

