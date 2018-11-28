# My custom/collected functions

add_origins <- function(x, method = c('numeric', 'categorical')) {
	
	if (method == 'numeric') {
    x1 <- merge(
		  add_origin_numeric(x, 'temp.level') %>% rename(idT = id),
		  add_origin_numeric(x, 'precip.level') %>% rename(idP = id)
	  )
  } else {
    x1 <- merge(
      add_origin_categorical(x, 'temp.level') %>% rename(idT = id),
      add_origin_categorical(x, 'precip.level') %>% rename(idP = id)
    )
  }

  x1 <- left_join(x, x1, by = c("site", "sp", "stage", "abun"))
  	
  return(x1)

}

add_origin_categorical <- function(x, y = c('temp.level','precip.level')) {
  # Add putative climate origins to list of site/species
   
  if(y == 'temp.level') clab <- c('Cooler','Warmer','Same temperature')
  if(y == 'precip.level') clab <- c('Drier','Wetter','Same precipitation')
  
  y <- c('site', y)
  
  tmp <- site.meta[, y]
  names(tmp)[2] <- 'lev'
  
  #add climate column
  x <- left_join(x, tmp, by = 'site')
  
  #create set of local communities
  #don't count species labeled as transients (id = 'Transient')
  loc <- filter(x, stage == 'mature' & id == 'Persistent' & abun > 0)
    
  # Here, if a species is present in both lower and higher climates, it is considered to be from the same climate. Kind of conservative but makes sense. 
  for (i in c(1:nrow(x))) {
    if (x$id[i] != 'Persistent') {
      if (x$sp[i] %in% filter(loc, lev < x$lev[i])$sp) x$id[i] <- clab[1]
      if (x$sp[i] %in% filter(loc, lev > x$lev[i])$sp) x$id[i] <- clab[2]
      if (x$sp[i] %in% filter(loc, lev == x$lev[i])$sp) x$id[i] <- clab[3]
      if (x$sp[i] %in% filter(loc, lev < x$lev[i])$sp &
          x$sp[i] %in% filter(loc, lev > x$lev[i])$sp) x$id[i] <- clab[3]
    }
  }
  
  # clean up remaining categories.
  x$lev <- NULL
  x$id[x$id == 'Persistent'] <- 'Local'
  x$id[x$id == 'Transient'] <- 'Unknown'
  
  return(x)
}

add_origin_numeric <- function(x, y = c('temp.level','precip.level')) {
  
  # Add putative climate origins to list of site/species   
  y <- c('site', y)
  
  tmp <- site.meta[, y]
  names(tmp)[2] <- 'lev'
  
  #add climate column
  x <- left_join(x, tmp, by = 'site')
  
  #create set of local communities
  #don't count species labeled as transients (id = 'Transient')
  loc <- filter(x, stage == 'mature' & id == 'Persistent' & abun > 0)
    
  # Here, if a species is present in both lower and higher climates, it is considered to be from the same climate. Kind of conservative but makes sense. 
  for (i in c(1:nrow(x))) {
    if (x$id[i] != 'Persistent' & x$sp[i] %in% loc$sp) {
      if (x$sp[i] %in% filter(loc, lev < x$lev[i])$sp) x$id[i] <- max(filter(loc, sp == x$sp[i])$lev - x$lev[i])
      if (x$sp[i] %in% filter(loc, lev > x$lev[i])$sp) x$id[i] <- min(filter(loc, sp == x$sp[i])$lev - x$lev[i])
      if (x$sp[i] %in% filter(loc, lev == x$lev[i])$sp) x$id[i] <- 0
      if (x$sp[i] %in% filter(loc, lev < x$lev[i])$sp &
          x$sp[i] %in% filter(loc, lev > x$lev[i])$sp) x$id[i] <- 0
    }
  }
  
  # clean up remaining categories.
  x$lev <- NULL
  x$id[x$id == 'Persistent'] <- 0
  x$id[x$id == 'Transient'] <- NA
  
  return(x)
}

loadpax <- function(pkg){
  # (1) checks package installation, (2) installs them if not, then (3) loads them
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

stat_sum_df <- function(fun, geom = 'crossbar', ...) {
  # For plotting
  stat_summary(fun.data = fun, geom = geom, width = 0.2, ...)
}


plot_it <- function(plot, name, dir = "", height, width, res = 300) {
  # custom function to plot a high quality image.
  #to alter quality, increase res
  
  png(filename = paste0(dir, name, '.png'), units = 'in',
      type = "cairo", width = width, height = height, res = res)
  
  #only works for ggplot. base plot() is dumb
  plot(plot)
  
  #close out any lingering devs...
  while(dev.cur() != 1) dev.off()
  
}

# predict loess curves. 
#I do this so I can make sure loess preditions don't exceed 100% -- also so confidence intervals don't plot beyond 100%
predmod_log<- function(x, y) {
  
  dat <- data.frame(x = jitter(x), y = y) # jitter to improve loess fit
  mod <- loess(y ~ log10(x), data = dat)
  xrange <- range(dat$x)
  xseq <- seq(from = xrange[1], to = xrange[2], length = 1000)
  pred <- predict(mod, newdata = data.frame(x = xseq), se=TRUE)
  y <- pred$fit
  ci <- pred$se.fit * qt(0.95 / 2 + .5, pred$df)
  ymin = y - ci
  ymax = y + ci
  pred_df <- data.frame(x = xseq, y = y, ymin, ymax, se = pred$se.fit)
  pred_df <- pred_df %>%
    mutate(
      y = ifelse(y > 1, 1, y),
      ymin = ifelse(ymin < 0, 0, ymin),
      ymax = ifelse(ymax > 1, 1, ymax))
  
  return(pred_df)
}

probfixes = function (vec, probs, fixes) {
  # A function that resolves any naming inconsitencies in a vector (vec)

  for(i in 1:length(probs)) {
    vec <- ifelse(as.character(vec) == as.character(probs[i]), as.character(fixes[i]), as.character(vec))
  }
  return(vec)
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  # A function that lets you put multiple plots together and have a single shared legend (from the first plot)
  # from hadley on the internet...
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)

}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# stage labels
stage_lvs <- c('rain' = 'Seed rain','bank' = 'Seed bank', 'seed' = 'All seeds',
	'germ' = 'Emerged','est' = 'Established','mature' = 'Mature vegetation')
stage_labels <- function(s, lvs = stage_lvs) {
  #add labels for stages
  s <- as.vector(lvs[match(s, names(lvs))])
  s <- factor(s, levels = lvs[lvs %in% s])
  return(s)
}


#subsample seed bank community down to a stated fraction
subbank <- function(x, frac) {

	set.seed(7)
	tmp <- x %>% filter(stage == 'bank')
	subsampled_bank <- tmp[0, ]

	for (i in unique(tmp$site)) {
	  tmp0 <- filter(tmp, site == i)
	  tmp0 <- tmp0[rep(c(1:nrow(tmp0)), times = tmp0$abun), ]
	  tmp0 <- tmp0[sample(c(1:nrow(tmp0)), size = round(nrow(tmp0) * frac)), ]
	  tmp0$abun <- 1
	  subsampled_bank <- rbind(subsampled_bank, tmp0)
	}
  
	subsampled_bank <- subsampled_bank %>%
	  group_by(site, stage, sp, id) %>%
	  summarise(abun = sum(abun))

	x <- rbind(as.data.frame(filter(x, stage != 'bank')), 
	           as.data.frame(subsampled_bank))

	return(x)

}

#merge seed rain and seed bank
mergeseeds <- function(x) {

	x <- x %>%
	  mutate(stage = ifelse(stage %in% c('rain','bank'), 'seed', stage)) %>%
	  group_by(site, stage, sp, id) %>%
	  summarise(abun = sum(abun)) %>%
	  ungroup()

	return(x)

}


# print list of loaded functions
print(data.frame(Custom_Functions = 
  c('add_origins: add putative climate origins',
    'loadpax: install+load multiple packages',
    'probfixes: corrects taxonomic inconsitencies',
    'grid_arrange_shared_legend: Multiple plots, one legend',
    'multiplot: multiple plots into one',
    'stage_labels: label and factorize stages')))
