#' Retrieve Colors for ACR Subgroups and Other Variables
#'
#' Returns a named vector of colors associated with subgroups, sex, and case-control status.
#'
#' @param sg1_var_name Name for subgroup 1 (default: `"1"`)
#' @param sg2_var_name Name for subgroup 2 (default: `"2"`)
#' @param sg3_var_name Name for subgroup 3 (default: `"3"`)
#' @param sg4_var_name Name for subgroup 4 (default: `"4"`)
#' @param fem_var_name Name for Female (default: `"Female"`)
#' @param mas_var_name Name for Male (default: `"Male"`)
#' @param case_var_name Name for Cases (default: `"case"`)
#' @param control_var_name Name for Controls (default: `"control"`)
#' @return A named vector of hex color codes.
#' @examples
#' subgroup_colors = acr_subgroups_paper_colors()
acr_subgroups_paper_colors <- function(
    sg1_var_name = "1", sg2_var_name = "2", sg3_var_name = "3", sg4_var_name = "4",
    fem_var_name = "Female", mas_var_name = "Male", case_var_name = "case", control_var_name = "control"
) {
  #colors <- c("#3d7196", "#cf823e", "#438943", "#b34b4c",
  #            "#B983A7", "#C8D9F2", "#C16840", "#7f7f7f")

  colors <- c("#72a3c7", '#f5cd87', '#588d7a', '#ff876f',
              "#B983A7", "#C8D9F2", "#C16840", "#7f7f7f")
  names(colors) <- c(sg1_var_name, sg2_var_name, sg3_var_name, sg4_var_name,
                     fem_var_name, mas_var_name, case_var_name, control_var_name)
  return(colors)
}

