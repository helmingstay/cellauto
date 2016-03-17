require(cgolr)
obj.dim <- c(400, 600)
not.used <- cgolr_settings(rule_by_name(name='life'))
not.used <- cgolr_settings(color_blues(decay=0.1))

## color palette functions from settings.R
allowed_cols <- list(
    `B&W`=color_bw, 
    Reds=color_reds,
    Blues=color_blues,
    RGB=color_rgb
)
