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
