# A function to mimic a tooltip using tcltk only
# text - the content of the tool tip
# targetWidget - the widget to which the tooltip is going to be associated
# width - the width of the tooltip measured as pisels.

tooltip <- function(text, targetWidget, width = 350){

    end <- function(){
        tkdestroy(base)
    }

    tipX <- as.numeric(tkwinfo("rootx", targetWidget)) +
            as.numeric(tkwinfo("width", targetWidget))
    tipY <- as.numeric(tkwinfo("rooty", targetWidget))

    # Takes out the frame and title bar
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    on.exit(tkdestroy(base))
    # Put the TW in the right place
    tkwm.geometry(base, paste("+", tipX, "+", tipY, sep = ""))
    tip <- tklabel(base, text = text, background = "white",
                   wraplength = width)
    tkpack(tip)

    tkbind(targetWidget, "<Leave>", end)

    tkwait.window(base)

    return(invisible())
}
