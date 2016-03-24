library(cgolr)
the_package_name <- 'cgolr'
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

app_dir <- paste0(path.package(the_package_name), '/webapp')
theme_dir <- paste0(app_dir, '/themes')
allowed_themes <- dir(theme_dir, patt='*bootstrap.min.css')
names(allowed_themes) <- sub('.bootstrap.min.css', '', allowed_themes)
## set theme here
cur_theme <- allowed_themes[3]

