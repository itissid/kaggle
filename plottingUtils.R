library(ggplot2)
library(colorspace)


plotMaps = function(shp.df, map.colors, grouping_var, fill_var, longitude_var_name="longitude", latitude_var_name="latitude" ) {
    # Map colors are based on fill and should be matchin the unique fill variable values
    grouping_var = rlang::enquo(grouping_var)
    grouping_var_str = rlang::quo_name(grouping_var)
    fill_var = rlang::enquo(fill_var)
    fill_var_str  = rlang::quo_name(fill_var)
    assertthat::assert_that(length(map.colors) == shp.df %>% dplyr::distinct(!!fill_var) %>% nrow)

    p1 = ggplot2::ggplot(data=shp.df) +
        ggiraph::geom_polygon_interactive(aes_string(longitude_var_name, latitude_var_name, group=grouping_var_str, fill=fill_var_str, 
                                              tooltip=fill_var_str), color="black") +
        scale_fill_manual(values=map.colors) +
        theme(legend.position="none", legend.text=ggplot2::element_text(size=6)) 
    
    #p2 = p1 +facet_wrap(as.name(group, scales=c("free"))
    return(p1)
    #ggiraph::ggiraph(code = {print(p1)}, zoom_max = 10)
    #print(p1) 
}


plotMapsWithPropertyData = function(shp.df, grouping_var, prop_data, prop_var, prop_shape_var) {
    #grouping_var_name = as.name(grouping_var)
    p1 = ggplot2::ggplot() +
        ggiraph::geom_polygon_interactive(aes_(~long, ~lat, group = as.name(grouping_var)), data=shp.df, color="red",fill="light grey") +
        ggiraph::geom_point_interactive(
            mapping=aes_(x=~long, y=~lat , color=as.name(prop_var), tooltip=as.name(prop_var),  shape = as.name(prop_shape_var), 
                         size=as.name(prop_var)), 
            data=prop_data, alpha=0.8) +
        scale_fill_gradient(low = muted("red"), high = muted("blue"), na.value = "grey50", guide="colourbar") +
        guides(color=guide_legend(), size=guide_legend(), alpha="none") +
        theme(legend.text=ggplot2::element_text(size=6))

    ggiraph::ggiraph(code = {print(p1)}, zoom_max = 10) 
}

pal.custom = function (n, h = c(175, 330), c = 100, l = c(29, 95), power = 1.01136363636364,
    fixup = TRUE, gamma = NULL, alpha = 1, ...)
{
    if (!is.null(gamma))
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L)
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- c[1L]
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L],
        C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L],
            h[2L])), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)),
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}

colorHelper = function(column) {
    u = unique(column)
    setNames(pal.custom(length(u)), u)
}
################### Utilities for diagnostics on Errors and Residuals ####################
errorsByMonth = function(transactons) {
    # Must contain the date, abs_logerror 
    transactions %>% 
      mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
      group_by(year_month) %>% summarize(mean_abs_logerror = mean(abs_logerror)) %>% 
      ggplot(aes(x=year_month,y=mean_abs_logerror)) + 
      geom_line(size=1.5, color="red")+
      geom_point(size=5, color="red")+theme_bw()

    transactions %>% 
      mutate(year_month = make_date(year=year(date),month=month(date)) ) %>% 
      group_by(year_month, overunder) %>% summarize(mean_abs_logerror = mean(abs_logerror)) %>% 
      ggplot(aes(x=year_month,y=mean_abs_logerror, color=overunder)) + 
      geom_line(size=1.5)+
      geom_point(size=5)+theme_bw()
}

corrplotWrapper = function (transactions) {
        # Correlation plots between all the complete observations and logerror; also the absolute log error
        corrplot::corrplot(cor(transactions, use="complete.obs"), type = "lower")
        corrplot::corrplot(cor(transactions %>% dplyr::mutate(abs_pred = abs(pred), abs_logerror = abs(logerror)), use="complete.obs"), type = "lower") 
}

conditionalResiudalPlot = function(transactions) {
    # Plotting the holdout data set residuals.
    p = ggplot(transactions) +
        facet_grid(region_county~.) +
        geom_point(aes(x=pred, y=resid), size=0.05)+
        ylim(-0.25,0.25) +
        xlim(-0.05, 0.05)+
        theme_bw()
    print(p)

}

source("eda.R") 

plotPercentileWrapper = function(data, var, plot_title, grid_lines_interactive=20, grid_ymax = 2) {
    var = rlang::enquo(var)
    gg = plotOutcomeDensityPercentiles(
        data, !!var, type, xlimit=NULL, grid_lines_interactive = grid_lines_interactive, grid_ymax = grid_ymax, plot_title=plot_title)
    ggiraph(code = {print(gg)}, hover_css = "stroke:red;")
}


############ FROM VTREAT #################################
#' Plot a conditional scatter plot with marginals.  xvar is the independent variable (input or model), and yvar is the dependent variable, and cvar is the condition code
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param cvar name of condition variable
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param annot_size numeric scale annotation text (if present)
#' @param colorPalette name of a Brewer palette (see http://colorbrewer2.org/ )
#' @param adjust_x  numeric adjust x density plot
#' @param adjust_y  numeric adjust y density plot
#' @examples
#'
#' set.seed(34903490)
#' frm = data.frame(x=rnorm(50),y=rnorm(50))
#' frm$cat <- frm$x+frm$y>0
#' WVPlots::ScatterHistC(frm, "x", "y", "cat",
#'                       title="Example Conditional Distribution")
#'
#' @export
ScatterHistC = function(frame, xvar, yvar, cvar, title, ...,
                        annot_size=3,
                        colorPalette="Dark2",
                       adjust_x = 1,
                       adjust_y = 1) {
  checkArgs(frame=frame,xvar=xvar,yvar=yvar,title=title,...)
  minimal_labels = TRUE

  # Use this plot to print the legend.
  if(is.factor(frame[[cvar]])) {
    labs = levels(frame[[cvar]])
    labs = factor(labs,
                  levels=labs,
                  labels=paste(" ", labs, sep='')  # add a blank to the front
    )
    # this preserves the factor order, if it's not alphabetical
  } else {
    # if frame[[cvar]] isn't a factor, then it will sort alphabetical anyway
    labs = levels(as.factor(frame[[cvar]]))
    labs = paste(" ", labs, sep='')  # add a blank to the front
  }
  nlab = length(labs)

  legendframe = data.frame(x=0, y=seq_len(nlab), cvar=labs)
  emptyframe = data.frame(x=c(-0.01,1), y=c(0,nlab+2))

  legendplt =  ggplot2::ggplot() +
    ggplot2::annotate("text", x=0.1, y=nlab+1, label=cvar, size=annot_size) +
    ggplot2::geom_point(data=legendframe, ggplot2::aes(x=x,y=y,color=cvar),
                        size=3) +
    ggplot2::geom_text(data=legendframe, ggplot2::aes(x=x,y=y,label=cvar),
                       nudge_x=0.025, hjust="left", size=annot_size) +
    ggplot2::geom_point(data=emptyframe, ggplot2::aes(x=x,y=y),
                        colour = "white") +
    ggplot2::theme(plot.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.margin = grid::unit(c(1, 1, 0, 0), "lines"),
                   legend.position="none") +
   ggplot2:: scale_color_brewer(palette=colorPalette)


  # scatterplot of x and y
  plot_center = ggplot2::ggplot(frame,
                                ggplot2::aes_string(x=xvar,y=yvar,color=cvar)) +
    ggplot2::geom_point() +
    ggplot2::theme(plot.margin = grid::unit(c(0, 0, 0, 0), "lines")) +
    ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)

  # get the data range, to help align plots
  x = frame[[xvar]]
  y = frame[[yvar]]
  xlims =  c(min(x), max(x))
  ylims =  c(min(y), max(y))

  #  print(xlims)
  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  plot_center = plot_center +
    ggplot2::coord_cartesian(xlim=xlims) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::theme(legend.position="none")

  # print(ggplot_build(plot_center)$panel$ranges[[1]]$x.range)

  # marginal density of x - plot on top
  #
  # 0,0,0,0 -- title squooshed down
  # 1,0,0,0 -- title has space
  # 0,1,0,0 -- right side is shorter
  # 0,0,1,0 -- bottom gap bigger
  # 0,0,0,1 -- left side is shorter
  #
  plot_top <- ggplot2::ggplot(frame,
                              ggplot2::aes_string(x=xvar,color=cvar)) +
    ggplot2::geom_line(stat='density',adjust=adjust_x) +
    ggplot2::coord_cartesian(xlim=xlims) +
     ggplot2::scale_x_continuous(expand = c(0,0))
  if(minimal_labels) {
    plot_top = plot_top +
      ggplot2::theme(legend.position = "none",
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  } else {
    plot_top = plot_top +
      ggplot2::theme(plot.margin = grid::unit(c(1, 0, 0, 0), "lines"))
  }
  plot_top <- plot_top + ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)


  # marginal density of y - plot on the right
  plot_right <- ggplot2::ggplot(frame,
                                ggplot2::aes_string(x=yvar,color=cvar)) +
    ggplot2::geom_line(stat='density', adjust=adjust_y) +
    ggplot2::coord_cartesian(xlim=ylims) +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::coord_flip(xlim=ylims, expand=0)
  if(minimal_labels) {
    plot_right = plot_right +
      ggplot2::theme(legend.position = "none",
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  } else {
    plot_right = plot_right +
      ggplot2::theme(plot.margin = grid::unit(c(0, 1, 0, 0), "lines"))
  }
  plot_right <- plot_right + ggplot2::scale_color_brewer(palette=colorPalette) +
    ggplot2::scale_fill_brewer(palette=colorPalette)

  # esimate size
  yPadFn <- designYLabelPadFunction(plot_center +
                                      ggplot2::scale_y_continuous(limits=ylims, expand = c(0,0)),
                                    plot_top)
  # adjust using estimte
  plot_center <- plot_center +
    ggplot2::scale_y_continuous(limits=ylims, label=yPadFn, expand = c(0,0))
  plot_top <- plot_top +
    ggplot2::scale_y_continuous(label=yPadFn)

  # arrange the plots together, with appropriate height and width
  # for each row and column

  gridExtra::grid.arrange(plot_top, legendplt, plot_center, plot_right,
                          top=grid::textGrob(title),
                          ncol = 2, nrow = 2, widths = c(4,1), heights = c(1, 4))
}

ScatterHistN = function(frame, xvar, yvar, zvar, title, ...,
                        annot_size=3,
                        colorPalette="RdYlBu",
                        nclus=3,
                        adjust_x = 1,
                        adjust_y = 1) {
  checkArgs(frame=frame,xvar=xvar,yvar=yvar,title=title,...)
  q <- sort(unique(quantile(frame[[zvar]],seq(0, 1, 1/nclus))))
  yC <- cut(frame[[zvar]],q,include.lowest=TRUE)
  if(length(unique(yC))<=1) {
    q <- sort(unique(c(q,median(unique(frame[[zvar]])))))
    yC <- cut(frame[[zvar]],q,include.lowest=TRUE)
  }
  frame[[zvar]] <- yC
  ScatterHistC(frame, xvar, yvar, zvar, title, ...,
               annot_size=annot_size,
               colorPalette=colorPalette,
               adjust_x = adjust_x,
               adjust_y = adjust_y)
}
extractProjection <- function(ndim,princ) {
  # pull off the rotation.  
  proj <- princ$rotation[,1:ndim] 
  # sign was arbitrary, so flip in convenient form
  for(i in seq_len(ndim)) {
    si <- sign(mean(proj[,i]))
    if(si!=0) {
      proj[,i] <- proj[,i]*si
    }
  }
  proj
}
