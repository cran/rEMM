## the generic with x, y, ... comes from graphics
setMethod("plot", signature(x = "EMM", y = "missing"),
        function(x, y, method = c("MDS", "graph", "state_counts",
                        "transition_counts"), data = NULL, 
                parameter=NULL, ...){ 

            method <- match.arg(method)

            p <- .get_parameters(list(
                            state_counts=TRUE,
                            arrow_width=TRUE,
                            arrows = "counts",      ## or "probabilities"
                            arrow_width_multiplier=1,
                            state_size_multiplier=1,
                            add_labels = TRUE,
                            state_labels = NULL,
                            mark_clusters = TRUE,
                            mark_states = NULL,
                            draw_ellipses = FALSE,
                            nAttrs = list(),
                            eAttrs = list()
                            ), parameter)

            emm_centers <- state_centers(x)

            if(method=="state_counts") {
                barplot(sort(state_counts(x), decreasing=TRUE), 
                        ylab="State counts", ...)

            }else if(method=="transition_counts") {
                tr <- transitions(x)
                cnt <- transition(x, tr, type="counts")
                names(cnt) <- apply(tr, MARGIN=1, FUN = 
                        function(x) paste(x, collapse="->"))
                barplot(sort(cnt, decreasing=TRUE), 
                        ylab="Transition counts", ...)

            }else if(method=="graph") {
                if(!require("Rgraphviz")) stop ("Package Rgraphviz needed!")

                nAttrs <- p$nAttrs
                eAttrs <- p$eAttrs

                if(!is.null(p$mark_states)) {
                    nAttrs$color <- rep("red", length(p$mark_states))
                    names(nAttrs$color) <- p$mark_states
                    nAttrs$fontcolor <- rep("red", length(p$mark_states))
                    names(nAttrs$fontcolor) <- p$mark_states
                }

                ## vertex labels
                if(!is.null(p$state_labels)) {
                    names(p$state_labels) <- states(x)
                    nAttrs$label <- p$state_labels
                }

                if(!p$add_labels) {
                    p$state_labels <- rep("", size(x))
                    names(p$state_labels) <- states(x)
                    nAttrs$label <- p$state_labels
                }

                ## vertex size
                if(p$state_counts){
                    nAttrs$width <- .5 +
                    x@counts/max(x@counts)*p$state_size_multiplier
                }

                ## setting line width for arrows does not seem to be implemented
                pl <- plot(x@mm, recipEdges="distinct",
                        nodeAttrs = nAttrs, edgeAttrs = eAttrs, ...)
                if(p$arrow_width) {
                    ## redraw arrows with different width
                    ## calculate arrow length (see plot in graph.R in Rgraphviz)
                    agn <- AgNode(pl)
                    nodeDims <- sapply(agn, function(n)
                            { c(getNodeRW(n)+getNodeLW(n), getNodeHeight(n)) })

                    arrowLen <- par("pin")[1] / diff(par("usr")[1:2]) * 
                    min(nodeDims) / pi *1.3
                    ## I'm not quite sure why we have to make them 30% longer

                    ## lwd for arrows
                    edges <- AgEdge(pl)

                    lwd <- transition(x, sapply(edges, tail), sapply(edges, head), 
                            type=p$arrows)
                    ## normalize 
                    lwd <- lwd/max(lwd)
                    lwd <- 1 + lwd * 5 * p$arrow_width_multiplier

                    for(i in 1:length(edges)) {
                        lines(edges[[i]], lwd=lwd[i], len=arrowLen)
                    }
                } 
            }

            else {
                if(nrow(emm_centers)<3) stop('Less than 3 centers! Use plot_type="graph".')

                ## self transitions are not visible for these plots
                x <- remove_selftransitions(x)

                if(is.null(data)){

                    if(ncol(emm_centers)==2 && 
                            tolower(x@measure)=="euclidean" ) {
                        ## we need no MDS
                        mds <- list(points = emm_centers, GOF=c(0,1))
                    }else{
                        d <- dist(emm_centers, method=x@measure)
                        mds <- cmdscale(d, eig=TRUE, add=TRUE)
                    }
                    pts <- mds$points
                    dimnames(pts) <- list(states(x), NULL)

                    ## start plotting
                    plot(pts, xlab="Dimension 1", ylab="Dimension 2", type="n", ...,
                            sub= paste("These two dimensions explain",
                                    round(100 * mds$GOF[2], digits = 2), 
                                    "% of the point variability."))


                    ## use cex for point size
                    cex <- 2
                    if(p$state_counts) cex <- 
                    2+x@counts/max(x@counts) * p$state_size_multiplier*5

                    ## arrows
                    edges <- transitions(x)
                    arrows_fromto <- cbind(
                            from_x = pts[edges[,1],1], from_y = pts[edges[,1],2],
                            to_x = pts[edges[,2],1],to_y = pts[edges[,2],2]
                            )

                    ## make arrows shorter so they do not cover the nodes 
                    nodeRad2 <- cbind(
                            x=(sapply(cex, FUN=function(cx)  
                                            strwidth("o", cex=cx/1.2))/2)^2,
                            y=(sapply(cex, FUN=function(cx)
                                            strheight("o", cex=cx/1.2))/2)^2)

                    x2 <- (arrows_fromto[,3]-arrows_fromto[,1])^2
                    y2 <- (arrows_fromto[,2]-arrows_fromto[,4])^2
                    z2 <- x2+y2
                    shorten <- cbind(
                            x1=sqrt(nodeRad2[edges[,1],1]/z2 * x2),
                            y1=sqrt(nodeRad2[edges[,1],2]/z2 * y2),
                            x2=sqrt(nodeRad2[edges[,2],1]/z2 * x2),
                            y2=sqrt(nodeRad2[edges[,2],2]/z2 * y2)
                            )

                    signs <- matrix(1, ncol=2, nrow=nrow(shorten))
                    signs[arrows_fromto[,1] > arrows_fromto[,3],1] <- -1
                    signs[arrows_fromto[,2] > arrows_fromto[,4],2] <- -1
                    signs <- cbind(signs, signs*-1)

                    shorten <- shorten * signs
                    shorten[is.nan(shorten)] <- 0 ## take care of too short arrows
                    arrows_fromto <- arrows_fromto + shorten

                    ## lwd for arrows
                    lwd <- 1
                    if(p$arrow_width) {
                        lwd <- transition(x, edges[,1], edges[,2], type=p$arrows)
                        ## normalize 
                        lwd <- lwd/max(lwd)
                        lwd <- 1+ lwd * p$arrow_width_multiplier*5
                    }

                    ## arrows whines about zero length arrows
                    suppressWarnings(
                            arrows(arrows_fromto[,1], arrows_fromto[,2], 
                                    arrows_fromto[,3],arrows_fromto[,4],
                                    length=0.15, col="grey", angle=20, lwd=lwd)
                            )

                    ## overplot points and text
                    points(pts, cex=cex,...)

                    if(p$add_labels) {
                        ## plot labels
                        if(is.null(p$state_labels)) labels <- states(x)
                        else labels <- p$state_labels
                            cex <- cex/1.7
                        ## make sure double digit labels fit
                        cex <- cex * (strwidth("8")/
                                apply(cbind(strwidth(labels), strheight(labels)), 
                                        MARGIN=1, max))
                        text(pts, labels=labels, cex=cex)
                    }

                } else {
                    ## project state centers onto dataset
                    if(ncol(emm_centers)==2 && 
                            tolower(x@measure)=="euclidean" ) {
                        ## we need no MDS
                        mds <- list(points = emm_centers, GOF=c(0,1))
                        centers <- emm_centers
                        allpoints <- data
                    
                    }else{

                        d <- dist(rbind(emm_centers, data), method=x@measure)
                        mds <- cmdscale(d, eig=TRUE, add=TRUE)
                        centers <- mds$points[1:nrow(emm_centers),]
                        allpoints <- mds$points[-c(1:nrow(emm_centers)),]
                    }
                    ## points
                    if(p$mark_clusters){
                        point_center <- find_states(x, data, match_state="exact")
                        ## make state name integer for pch
                        pch <- as.integer(factor(point_center, 
                                        levels = states(x)))
                        pch_center <- 1:size(x)
                        
                        ## make sure we stay below 25 for pch
                        while(any(pch>25, na.rm=TRUE)) {
                            reduce <- pch>25
                            reduce[is.na(reduce)] <- FALSE
                            pch[reduce] <- pch[reduce] -25
                        }
                        while(any(pch_center>25)) pch_center[pch_center>25] <-
                            pch_center[pch_center>25] -25


                        plot(allpoints, xlab="Dimension 1", ylab="Dimension 2", 
                                col="grey", pch=pch, ...,
                                sub= paste("These two dimensions explain",
                                        round(100 * mds$GOF[2], digits = 2),
                                        "% of the point variability."))

                        ## plot points which do not belong to any state
                        if(any(is.na(point_center)))
                            points(allpoints[is.na(point_center),,drop=FALSE], 
                                col="blue", pch=1)

                        ## add ellipses
                        ## FIXME: does not work with d>2
                        if(p$draw_ellipses) {
                            library(sfsmisc)
                            tmp <- lapply(1:size(x),
                                    FUN = function (i) {
                                        thr <- x@var_thresholds[i]
                                        loc <- state_centers(x)[i,]
                                        lines(ellipsePoints(thr, thr, loc=loc), 
                                                col = "black", lty=2)
                                    })
                            }


                    }else{
                        plot(allpoints, xlab="Dimension 1", ylab="Dimension 2", 
                                col="grey", ...,
                                sub= paste("These two dimensions explain",
                                        round(100 * mds$GOF[2], digits = 2),
                                        "% of the point variability."))
                    }

                    cex <- 1

                    ## use cex for point size (scale: 1...3)
                    if(p$state_counts) cex <- 1+x@counts/max(x@counts)*2

                    ## centers
                    if(p$mark_clusters) points(centers, 
                            col="red", pch=pch_center, cex=cex)
                    else points(centers, col="red", pch="+", cex=cex)
                    #points(centers, col="red", pch=1:size(x), cex=cex)
                    if(p$add_labels) text(centers, labels=states(x), pos=3)
                }

            }
        }
        )
