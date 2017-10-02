### EDA routines to help performance ###

getOutcomePercentiles = function(transactions, outcome, percentiles=c(best=1,typical=3,worst=5) ) {
    # return the percentiles(specified on a scale of [1,5]) of the outcome column in the 
    # transaction data frame. Plotting the percentiles in color coded graphs gives you an
    # idea of the trend of the outcome variable against any-other. Typically the outcome is the
    # logerror or the residual
    outcome = rlang::enquo(outcome)
    transactions <- transactions %>% dplyr::mutate(
        percentile = cut(
            !!outcome,
            quantile(!!outcome, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1), names = FALSE),
            include.lowest = TRUE,labels=FALSE))
    percentileData = list()
    for(i in names(percentiles)) {
        tmp1 <- transactions %>% 
            dplyr::filter(percentile == percentiles[[i]])  %>%
            dplyr::mutate(type=i)
        assertthat::assert_that(nrow(transactions) > nrow(tmp1))
        assertthat::assert_that(nrow(tmp1) > 0)
        percentileData[[percentiles[[i]]]] = tmp1
    }
    return(percentileData)
}

plotOutcomeDensityPercentiles = function(tmp, x_var, fill_var, xlimit, grid_lines_interactive=10, grid_ymax=2, plot_title="default title") { 
    col_pal <- "Set1"
    x_var= rlang::enquo(x_var)
    x_var_str = rlang::quo_name(x_var)
    fill_var= rlang::enquo(fill_var)
    fill_var_str = rlang::quo_name(fill_var)
    # Code for drawing X equally spaced grid lines
    x_range = seq(min(tmp[, x_var_str]), max(tmp[, x_var_str]), length.out=grid_lines_interactive)
    rep_each = as.integer(nrow(tmp)/length(x_range)) + 1
    gridlines =  rep(x_range, each=rep_each)
    assertthat::assert_that(length(gridlines) >= nrow(tmp))
    gridlines = gridlines[1:nrow(tmp)]
    gridlines = data.frame(x=x_range, xend=x_range, y=0, yend=grid_ymax) 

    p = tmp %>% 
        ggplot() + 
        geom_line(mapping=aes_string(x=x_var_str,  fill=fill_var_str, color=fill_var_str), stat="density", size=1.2) + 
        theme_bw() +
        scale_fill_brewer(palette=col_pal)+
        scale_color_brewer(palette=col_pal)+
        ggtitle(plot_title)

    p = p + ggiraph::geom_segment_interactive(
            data=gridlines,
            mapping=aes(x=x, y=y, xend=xend, yend=yend, tooltip=x), 
            alpha = 0.7, color="gray", size=1)
    if(!is.null(xlimit))
        p = p + xlim(xlimit)
    return(p) 
}

plot.ridges.overunder = function(X, colname.feature, colname.factor, facet.feature=NA, xrange=NA) {
    col.feature.enquo = enquo(column.feature)
    col.factor.enquo = enquo(column.factor)
    f = c(colname.feature, colname.factor, "overunder") 
    f = if(is.na(facet.feature)) f else c(f, facet.feature)
    X %<>% dplyr::select_at(dplyr::vars(f)) %>% na.omit
    xrange = ifelse(is.na(xrange), range(unlist(X %>% pull(!!col.feature.enquo))), xrange)

    p = ggplot(X) +
           geom_density_ridges(aes_string(x = colname.feature, y = colname.factor, fill="overunder", scale=2), alpha=0.3) +
           theme_ridges() + xlim(xrange)
    if(!is.na(facet.feature)) {
        p = p + facet_grid(paste("~", facet.feature))
    }
    print(p)
}

plot.ridges = function(X, colname.feature, colname.factor) {
    col.feature.enquo = enquo(column.feature)
    col.factor.enquo = enquo(column.factor)
    p = ggplot(X) +
            geom_density_ridges(aes_string(x = colname.feature, y = colname.factor), alpha=0.3) +
            scale_y_discrete(expand = c(0.1, 0))
            theme_ridges()
    print(p)
}

plot.binnedResidual.vs.predictor = function(
                        X, predictors, error.colnames=c("logerror","residual"), 
                        nbins=50, cardinality_threshold=15, colsperrow=5) {
    # Row1: all the predictors vs there logerrors with logerror binned and averaged over each bin [as recommended by gelman pg 97 sec 5.6]
    # Row2: all the predictors vs their log residuals in same binning manner as Row 1
    # Designed for Zillow, but can be easily generaized
    # example call: plot.binnedResidual.vs.predictor(swiss, colnames(swiss)[2:6], error.colname="Fertility", resid.colname="Residual", nbins=2)
    for(f in error.colnames) {
        assertthat::assert_that(f %in% colnames(X))
    }
    # We can use the facet_wrap and along with resshape's melt we can plot the residuals for each layer

    f_binned_dens = function(data, mapping, ...) {
        # Custom ggplot2 function for plotting binned densities for continuos
        islike.factor = function(x){(is.factor(x) |is.character(x)) | length(unique(x)) < cardinality_threshold}
        if(as.character(mapping$y) %in% error.colnames) {
            y = data[[as.character(mapping$y)]]

        } else {
            print(paste("Column mapping should be one of:' ", paste(error.colnames, collapse=""), "' or '",
                        resid.colname, "' but was : '", mapping$y, "'. Aborting!", sep="" )) 
            stopifnot(TRUE)
        }
        x = data[[as.character(mapping$x)]]
        if(!islike.factor(x)) {
            h = hist(x, ifelse(is.na(nbins), "Sturges", nbins), plot=FALSE) # for binning y
            bin.idx = findInterval(x, h$breaks)
            data.x = h$mid[bin.idx] 
        } else {
            data.x =  factor(x)
        }
        #print(paste("bins = ", length(h$mids)))
        #print(paste(mapping, islike.factor(x)))
        data.errororresidual <- data.frame(x = data.x, y = y)
        p = ggplot(data.errororresidual, aes(x=x,y=y))
        if(!islike.factor(x)) {
              p = p + stat_summary(fun.y = "mean", geom = "point")
        } else {
             p = p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75, 0.95))
        }
        p = p + geom_hline(yintercept=0, color="red") +ylim(-0.45, 0.45)
            #scale_y_continuous(breaks = seq(-0.45, 0.45, by = 0.05)) 
        return(p)
        
    }
    div = as.integer(length(predictors)/colsperrow)
    plotlist = c()
    if(length(predictors) >= colsperrow) {
        for(i in 1:div) {
            p = ggduo(
                X, 
                columnsX=predictors[(5*(i-1)+1): (5*i)], 
                columnsY=error.colnames, 
                cardinality_threshold=cardinality_threshold, 
                types = list(continuous = f_binned_dens)
            ) + theme(axis.text.x=element_text(angle=45, size=8))
            print(p)
            plotList = c(p , plotlist)
        } 
    }
    if(length(predictors) %% colsperrow > 0) {
        p = ggduo(
            X, 
            columnsX=predictors[(5*div + 1): length(predictors)], 
            columnsY=error.colnames, 
            cardinality_threshold=cardinality_threshold, 
            types = list(continuous = f_binned_dens)
        ) + theme(axis.text.x=element_text(angle=45, size=3))
        print(p)
        plotList = c(p , plotlist)
    }
    plotlist
}

residualvsfitted= function(residuals, fitted) {
    ggplot(data.frame(residuals=residuals, fitted=fitted)) +
        geom_smooth(aes(y=residuals, x=fitted), size=0.5, method = "loess", span=0.3) +
        geom_point(aes(y=residuals, x=fitted), size=0.1, alpha=0.2)

}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {

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
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
