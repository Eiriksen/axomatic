#' Axomatic
#'
#' Used for defining x axis, y axis, and plot shape (square, rectangular, etc)
#' It uses functions axis_x() and axis_y() to make two axes, and then lets the user specify
#' the shape of the plot.
#' The function is used and added as a ggplot object.
#' See help page for axis_x() and axis_y().
#' @param x an x-axis specified with the function axis_x
#' @param y an y-axis specified with the function axis_y
#' @param ratio the ratio between the x and y axis. Defaults to 1 (a square)
#' @param scaleSize if the ratio is for physical graph size (T, default) or the values displayed. If physical (T), setting ration to 1 will always result in a square graph. If not physical (F), setting ration to 1 will give equal scale on both axes.
#' @export
#' @examples axomatic(x_axis(from=0,to=20,ticks=5,labels=1),y_axis(1000, 10000, 100, 2))
axomatic = function(x, y, ratio=1, scaleSize=T ){

  # Check if parameters are missing
  if (missing(x)) stop("X axis is not specified")
  if (missing(y)) stop("Y axis is not specified")

  # create scale ggplot objects
  if (scaleSize == T)  scale = coord_fixed(ratio*(x$limits[2]-x$limits[1])/(y$limits[2]-y$limits[1]))
  else                 scale = coord_fixed(ratio)

  # return ggplot objects
  return( list(y,x,scale) )
}


#' base_axis
#'
#' Internal function that's gives axis_y() and axis_x() their base functionality.
#' The function is used as a ggplot object.
#' @param xy Whether this is a x ("x") axis or y axis ("y").
#' @param from Start of the axis.
#' @param to End of the axis.
#' @param ticks How much space between each tickmark.
#' @param labels The numbers or text beneath the tickmarks. 0 = no labels, 1 = label on every tickmark, 5 = label every 5'th tickmark, and so on. Setting this paramter as a vector lets you make self-defined labels, e.g. you can write c("one","two","three","four") or whatever.
#' @param pad How much space before and after the first and last mark on the axis.
#' @param upperPad specification of pad, but only for space after the last mark.
#' @param lowerPad specifiction of pad, but only for spcae before the first mark.
#' @param trans_labels "none": Does nothing (default). Lognatural: Transforms numerical labels back to non-logarithmic form if the axis data is log transformed
#' @examples axis_x(from=0, to=20, ticks=1, labels=5, pad=2) #makes an axis going from 0 to 20, with 20 tickmarks, and labels every 5th tickmark, and a space of 2 before and after te marks
base_axis = function(xy, from, to, ticks=1, labels=1, pad=0, upperPad=NA, lowerPad=NA,trans_labels="none")
{

# From hereon we only use upperPad and lowerPad, but let pad owerwrite these if they're unspecified
  if (is.na(upperPad)) upperPad = pad
  if (is.na(lowerPad)) lowerPad = pad

  # What will be the upper and lower limits of the plot window
  upperLim=upperPad+to
  lowerLim=from-lowerPad

  # Some notes about language in this function:
  # v_breaks and v_labels are both vectors
  # v_breaks is a vector of numbers, each number becomes a BREAK (mark) on the axis
  # v_labels is a vector of LAVELS (the number/text below each mark), each entry is the label to the corresponding break (first label belongs to first break)
  # it is mostly these two vectors that define the axis as it is displayed


  # Rationale for following if/else hell: If "Ticks" are set to 0, there should be no tickmarks or labels at all, so set both to "null"
  # We need to check if "breaks" is a number or a premade vector
  # If it is a vector, then fine, we'll just use that vector as v_labels
  # If it is a number, then we need to convert that number to a vector of breaks (see code)
  if (ticks == 0)
  {
    v_labels = NULL
    v_breaks   = NULL
  }
  else
  {
    # define v_breaks
    v_breaks = seq(from,to,ticks)

    # check if "labels" is a vector or a number
    if(is.na(labels[2]))
    {
      # It's a number! :
      # check if labels are set to 0 (no labels)
      if (labels==0)
        # We still need to make "labels", but we're just going to make a vector of "empty" labels
        v_labels=rep("",length(v_breaks))
      # not zero, make labels every 'nth tickmark
      else
        v_labels=seq_minors(from,to,ticks,labels)
    }
    else
      # It's already a list of labels!
      v_labels=labels
  }

  if (trans_labels == "lognatural") v_labels = round(exp(numextract(v_labels)),2) %>% na_change("")

  #Depending on whether this is an x axis or y axis:
  if (xy == "x")
    return(scale_x_continuous(breaks=v_breaks, labels=v_labels, limits=c(lowerLim,upperLim)))
  else
    return(scale_y_continuous(breaks=v_breaks, labels=v_labels, limits=c(lowerLim,upperLim)))
}



#' axis_x
#'
#' Simplified function for defining the x axis with settings for tickmarks and labels.
#' This function is a slight simplification of the base_axis() function: It only works for the x axis.
#' The function is used as a ggplot object.
#' @param from Start of the axis.
#' @param to End of the axis.
#' @param ticks How much space between each tickmark.
#' @param labels The numbers or text beneath the tickmarks. 0 = no labels, 1 = label on every tickmark, 5 = label every 5'th tickmark, and so on. Setting this paramter as a vector lets you make self-defined labels, e.g. you can write c("one","two","three","four") or whatever.
#' @param pad How much space before and after the first and last mark on the axis.
#' @param upperPad specification of pad, but only for space after the last mark.
#' @param lowerPad specifiction of pad, but only for spcae before the first mark.
#' @param trans_labels "none": Does nothing (default). Lognatural: Transforms numerical labels back to non-logarithmic form if the axis data is log transformed
#' @export
#' @examples axis_x(from=0, to=20, ticks=1, labels=5, pad=2) #makes an axis going from 0 to 20, with 20 tickmarks, and labels every 5th tickmark, and a space of 2 before and after te marks
axis_x = function(from, to, ticks=1, labels=1, pad=0, upperPad=NA, lowerPad=NA, trans_labels="none")
{
  return(base_axis("x",from,to,ticks,labels,pad,upperPad,lowerPad,trans_labels))
}

#' axis_y
#'
#' Simplified function for defining the y axis with settings for tickmarks and labels.
#' This function is a slight simplification of the [base_axis()] function: It only works for the y axis.
#' The function is used as a ggplot object.
#' @param from Start of the axis.
#' @param to End of the axis.
#' @param ticks How much space between each tickmark.
#' @param labels The numbers or text beneath the tickmarks. 0 = no labels, 1 = label on every tickmark, 5 = label every 5'th tickmark, and so on. Setting this paramter as a vector lets you make self-defined labels, e.g. you can write c("one","two","three","four") or whatever.
#' @param pad How much space before and after the first and last mark on the axis.
#' @param upperPad specification of pad, but only for space after the last mark.
#' @param lowerPad specifiction of pad, but only for spcae before the first mark.
#' @param trans_labels "none": Does nothing (default). Lognatural: Transforms numerical labels back to non-logarithmic form if the axis data is log transformed
#' @export
#' @examples axis_y(from=0, to=20, ticks=1, labels=5, pad=2) #makes an axis going from 0 to 20, with 20 tickmarks, and labels every 5th tickmark, and a space of 2 before and after te marks.
axis_y = function(from, to, ticks=1, labels=1, pad=0, upperPad=NA, lowerPad=NA, trans_labels="none")
{
  return(base_axis("y",from,to,ticks,labels,pad,upperPad,lowerPad,trans_labels))
}

seq_minors=function(from, to, step, majors) {
  sequence = seq(from, to, step)
  sequence = round(sequence,2)
  return( every_nth(sequence,majors))
}

every_nth <- function(x, nth, empty = TRUE, inverse = TRUE) {
  # Source: User Maninal at Stackoverflow
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    }
    else {
      x[1:nth != 1]
    }
  }
  else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    }
    else {
      x[1:nth == 1]
    }
  }
}

