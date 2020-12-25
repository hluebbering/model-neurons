## Load all packages
library(kableExtra)
library(deSolve)
library(graphics)
library(mathjaxr)
library(pagedown)
library(phaseR)
library(knitr)
options(nat.plotengine = 'plotly')
library(nat)
library(rmarkdown)
library(captioner)
library(latex2exp)
library(RColorBrewer)
knitr::opts_chunk$set(echo = FALSE, out.width = 400, fig.align = "center", fig.pos = 'default', fig.show = "hold", warning = FALSE, message = FALSE)

## Auto-Number Figure Captions
fig_nums <- captioner(prefix = "Figure")
neur_cap <- fig_nums(name = "phase1", caption = "Neuronal communication. Projecting from each cell’s body, called its soma, is a tree of dendrites and a long axon. Dendrites receive the signals sent down the axon.")

knitr::include_graphics("assets/images/axon.jpg")
data("Cell07PNs")
xdata <- data("Cell07PNs")
head(xdata)

neur_cap2 <- fig_nums(name = "phase2", caption = "A Single Neuron. Dendrites are branched extensions (red to green dots) that can produce electrochemical simulations. The synapses distributed throughout the tree transmit electrical signals to the dendrites.")
plot(Cell07PNs[[1]], main=" ", cex=2)

ex.cs1 <- expression(plain("red nodes: branch points"),  paste("green nodes: end points"), paste("purple node: the soma"))  
legend(195,80, ex.cs1, pch = 24, cex = 1, pt.bg = c("indianred1", "green", "purple"), title="Type of Nodes", title.col = "honeydew3")
neur_cap3 <- fig_nums(name = "phase3", caption = "Dendrite Unique Morphology")

jBrewColors <- brewer.pal(n=5 , name = "PuBuGn")

# Multiple Neurons
plot(Cell07PNs[[1]], WithText=TRUE, col = "navy", lwd = 1, add=F, main=" ")
plot(Cell07PNs[2:6], col = jBrewColors, lwd = 1, add=T)
neur_cap6 <- fig_nums(name = "phase6", caption = "The myelin space on the myelin axon is un-insulated and has high-concentration ion channels: this allows the node to regenerate the action potential. Positive ion current is directed from inside to outside.")
knitr::include_graphics("assets/images/ganglion.jpg")
neur_cap4 <- fig_nums(name = "phase4", caption = "Synaptic transmission. When an action potential reaches the edge of an axon, it triggers an inrush of calcium ions, causing neurotransmitters' release into the synaptic cleft. These neurotransmitters diffuse through the postsynaptic cell membrane and activate receptors, usually causing currents to flow in the form of inward sodium currents.")
knitr::include_graphics("assets/images/axon2.jpg")
tables <- captioner(prefix = "Table", levels = 2)
table_cap1 <- tables("1", caption = "Defining the System's Functions")
functions <- data.frame("functions"= 
                          c("$\\bf \\mathcal{S}(x)$",
                            "$\\bf x=x(t)$",
                            "$\\bf D(\\theta)$",
                            "$\\bf C(w)$"
                          ),
                        "description" =
                          c("sigmoidal subpopulation response function",
                            "average level of excitation within a subpopulation",
                            "distribution of individual neural thresholds",
                            "distribution of the number of afferent synapses per cell"))
knitr::kable(functions ,col.names = c(" ", " "), caption = "Defining the System's Functions") %>%
  kable_styling(full_width = FALSE, font_size = 10) %>% 
  row_spec(0, color = "darkslategray") %>%
  column_spec(1, color = "white", background = spec_color(1:nrow(functions), begin=0.5, end = 0.95, option = "A", direction = -1, alpha = 0.8), popover = paste("functions:", functions$functions[1:nrow(functions)]), bold = T) %>%
  add_header_above(c("System Functions"=2),align = "l", line = F)
fig_nums <- captioner(prefix = "Figure")
body_cap <- fig_nums(name = "phase1", caption = "Logistic Growth Model. Plot of the flow field, horizontal lines at any equilibrium points, several trajectories for the case $\\beta=1$ and $K=4$.")
logistic_flowField  <- flowField(logistic,
                                 xlim       = c(0, 12),
                                 ylim       = c(-0.5, 4.5),
                                 parameters = c(1, 4),
                                 system     = "one.dim",
                                 frac = 2,
                                 add        = FALSE, cex.axis=0.01,
                                 points = 14,
                                 col = "gray50",
                                 lwd=0.8, 
                                 main="Flow Field and Nullclines",
                                 cex.main=0.8
)
grid()

logistic_nullclines <- nullclines(logistic,
                                  xlim       = c(0, 12),
                                  ylim       = c(-1, 4.5),
                                  parameters = c(1, 4),
                                  col = "aquamarine3", 
                                  lwd = 2.5,
                                  system     = "one.dim")


logistic_flowField  <- flowField(logistic,
                                 xlim       = c(0, 12),
                                 ylim       = c(-0.5, 4.5),
                                 parameters = c(1, 4),
                                 system     = "one.dim",
                                 frac = 2,
                                 add        = FALSE, 
                                 cex.axis=0.01,
                                 points = 14,
                                 col = "gray75",
                                 lwd=0.8, 
                                 main="Trajectories for various initial points", cex.main=0.8)
grid()
logistic_trajectory <- trajectory(logistic,
                                  y0         = c(0.001, 0.004, 0.04, 0.25, 1, 2),
                                  tlim       = c(0, 12),
                                  parameters = c(1, 4),
                                  col="darkslateblue",
                                  lty=1,
                                  system     = "one.dim")
body_cap2 <- fig_nums(name = "phase2", caption = "Sigmoid Subpopulation Response Function. The particular function shown here is the logistic curve: $S(x)=\\frac{1}{1+e^{-a(x-\\theta)}},$ such that $\\theta=5, a=1$. $X$ is average level of excitation in threshold units.")
sigmoid = function(x,a,b) {
  1 / (1 + exp(-a*(x-b)))
}
x <- seq(-5, 5, 0.01)
plot(x, sigmoid(x, a=1, b=1), col='blue', ylab="Sigmoid Function", xlab="X")
grid()

# nonlinear functions
Se = function(x) {
  aE = 1.3
  thrE = 4
  sigmoid(x, thrE, aE) - sigmoid(0, thrE, aE)
}

Si= function(x){
  aI = 2
  thrI = 3.7
  sigmoid(x, thrI, aI) - sigmoid(0, thrI, aI)
}
body_cap3 <- fig_nums(name = "phase3", caption = "Plot of the Sigmoid functions $S_e$ and $S_i$ for the excitatory and inhibitory subpopulations, respectively. Parameters: $a_e = 1.3, \\theta_e=4, a_i=2, \\text{ and } \\theta_i=3.7$")

y1 <- Se(x)
y2 <- Si(x)
## Not run: 
plot(x, y1, type = "l", col = "darkblue", 
     xlab = "", ylab = "", main = "Sigmoid Function(s)", xlim=c(-2,5), lwd=2)
lines(x, y2, col = "darkgreen", lwd=2)
grid()
legend(-1,0.8, c(TeX("$S_{excitatory}$"),TeX("$S_{inhibitory}$")), lty = 1,lwd=4, col = c("darkblue", "darkgreen"), title=TeX("$S = \\frac{1}{(1 + e^{-a (x-\\theta)})}$"), box.lwd = 0)
table_cap2 <- tables("2", caption = "Defining the System's Parameters and Functions")
parameters <- data.frame("parameters"= 
                           c("$\\bf c_1$",
                             "$\\bf c_2$",
                             "$\\bf c_3$",
                             "$\\bf c_4$",
                             "$\\bf P$", 
                             "$\\bf Q$"),
                         "description" = c(
                           "excitatory coupling constant\n applied to excitatory subpopulation",
                           "inhibitory coupling constant\n applied to excitatory subpopulation",
                           "excitatory coupling constant\n applied to inhibitory subpopulation",
                           "inhibitory coupling constant\n applied to inhibitory subpopulation",
                           "external input to excitatory cells",
                           "external input to inhibitory cells"
                         )
)

functions <- data.frame("functions"= 
                          c("$E(t)$",
                            "$I(t)$",
                            "$\\alpha(t-t^\\prime)$"),
                        "description" =
                          c("external input to excitatory subpopulation",
                            "external input to inhibitory subpopulation",
                            "stimulation decay function"))

knitr::kables(list(kbl(parameters,col.names = c(" ", " "), booktabs = T,linesep = "", escape = F) %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "hotpink") %>%
                     column_spec(1, color = "white", background = spec_color(mtcars$mpg[1:nrow(parameters)], begin = .2 ,end = 0.7), popover = paste("parameters:", parameters$parameters[1:nrow(parameters)])) %>%
                     add_header_above(c("System Parameters" = 2), align = "l", line = F),
                   
                   kbl(functions ,col.names = c(" ", " "), booktabs = T, linesep = "") %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "darkslategray") %>%
                     column_spec(1, color = "white", background = spec_color(1:nrow(functions), begin=0.5, end = 0.95, option = "A", direction = -1, alpha = 0.8), popover = paste("functions:", functions$functions[1:nrow(functions)]), bold = T) %>%
                     add_header_above(c("System Functions"=2),align = "l", line = F)), caption = "Defining the System's Parameters and Functions") %>%
  kable_styling(c("striped", "condensed"), full_width = F, position = "c", repeat_header_continued = T, latex_options = "striped") %>%
  kable_paper(full_width = F)
body_cap4 <- fig_nums(name = "phase4", caption = "Axonal and Dendritic geometry. Plot of Several Neurons to illustrate the diversity in axonal and dendritic geometry between excitatory and inhibitory cell types.")
plot(Cell07PNs[1])
plot(Cell07PNs[1:2])
plot(Cell07PNs[1:10])
plot(Cell07PNs[1:20])
se <- function(x){
  ae=1.2
  theta_e=2.8
  1/(1+exp(-ae*(x-theta_e)))
}

si <- function(x){
  ai=1
  theta_i=4 
  1/(1+exp(-ai*(x-theta_i)))
}
parameters <- data.frame("parameters"= 
                           c("$\\bf c_1$",
                             "$\\bf c_2$",
                             "$\\bf c_3$",
                             "$\\bf c_4$",
                             "$\\bf P(t)$", 
                             "$\\bf Q(t)$"),
                         "description" = c(
                           "excitatory coupling constant\n applied to excitatory subpopulation",
                           "inhibitory coupling constant\n applied to excitatory subpopulation",
                           "excitatory coupling constant\n applied to inhibitory subpopulation",
                           "inhibitory coupling constant\n applied to inhibitory subpopulation",
                           "external input to excitatory cells",
                           "external input to inhibitory cells"
                         )
)

functions <- data.frame("functions"= 
                          c("$E(t)$",
                            "$I(t)$",
                            "$\\mathcal{S}_e(x)$",
                            "$\\mathcal{S}_i(x)$"),
                        "description" =
                          c("external input to excitatory subpopulation",
                            "external input to inhibitory subpopulation",
                            "sigmoidal response function for excitatory subpopulation",
                            "sigmoidal response functions for inhibitory subpopulation"))


table_cap3 <- tables("3", caption = "Defining the System's Parameters and Functions")
table_cap4 <- tables("4", caption = "Defining the System's Parameters and Functions")
parameters <- data.frame("parameters"= 
                           c("$\\bf c_1$",
                             "$\\bf c_2$",
                             "$\\bf c_3$",
                             "$\\bf c_4$",
                             "$\\bf P(t)$", 
                             "$\\bf Q(t)$",
                             "$\\bf \\tau$, $\\bf \\tau^\\prime$"),
                         "description" = c(
                           "excitatory coupling constant\n applied to excitatory subpopulation",
                           "inhibitory coupling constant\n applied to excitatory subpopulation",
                           "excitatory coupling constant\n applied to inhibitory subpopulation",
                           "inhibitory coupling constant\n applied to inhibitory subpopulation",
                           "external input to excitatory cells",
                           "external input to inhibitory cells",
                           "response delays, after which cells at time t will be firing"
                         )
)


functions <- data.frame("functions"= 
                          c("$E(t)$",
                            "$I(t)$",
                            "$E(t + \\tau)$",
                            "$I(t + \\tau)$",
                            "$\\mathcal{S}_e(x)$",
                            "$\\mathcal{S}_i(x)$",
                            "$\\alpha(t-t^\\prime)$"
                          ),
                        "description" =
                          c("external input to excitatory subpopulation",
                            "external input to inhibitory subpopulation",
                            "proportion of excitatory cells firing per unit time at $t + \\tau$",
                            "proportion of inhibitory cells firing per unit time at $t + \\tau$",
                            "sigmoidal response function for excitatory subpopulation",
                            "sigmoidal response functions for inhibitory subpopulation",
                            "stimulation decay function"))

knitr::kables(list(kbl(parameters,col.names = c(" ", " "), booktabs = T,linesep = "", escape = F) %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "hotpink") %>%
                     column_spec(1, color = "white", background = spec_color(mtcars$mpg[1:nrow(parameters)], begin = .2 ,end = 0.7), popover = paste("parameters:", parameters$parameters[1:nrow(parameters)])) %>%
                     add_header_above(c("System Parameters" = 2), align = "l", line = F),
                   
                   kbl(functions ,col.names = c(" ", " "), booktabs = T, linesep = "") %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "darkslategray") %>%
                     column_spec(1, color = "white", background = spec_color(1:nrow(functions), begin=0.5, end = 0.95, option = "A", direction = -1, alpha = 0.8), popover = paste("functions:", functions$functions[1:nrow(functions)]), bold = T) %>%
                     add_header_above(c("System Functions"=2),align = "l", line = F)), caption = "Defining the System's Parameters and Functions") %>%
  kable_styling(c("striped", "condensed"), full_width = F, position = "c", repeat_header_continued = T, latex_options = "striped") %>%
  kable_paper(full_width = F)
parameters <- data.frame("parameters"= 
                           c("$\\bf c_1$",
                             "$\\bf c_2$",
                             "$\\bf c_3$",
                             "$\\bf c_4$",
                             "$\\bf P$", 
                             "$\\bf Q$",
                             "$\\bf k_e$",
                             "$\\bf k_i$",
                             "$\\bf \\tau_e$, 
                             $\\bf \\tau_i$",
                             "$\\bf r_e$",
                             "$\\bf r_i$"),
                         "description" = c(
                           "excitatory coupling constant\n applied to excitatory subpopulation",
                           "inhibitory coupling constant\n applied to excitatory subpopulation",
                           "excitatory coupling constant\n applied to inhibitory subpopulation",
                           "inhibitory coupling constant\n applied to inhibitory subpopulation",
                           "external input to excitatory cells",
                           "external input to inhibitory cells",
                           "non-dimensional constant for $\\mathcal{S}_e \\lt 1$",
                           "non-dimensional constant for $\\mathcal{S}_i \\lt 1$",
                           "time constants",
                           "refractory delay for excitatory subpopulation",
                           "refractory delay for inhibitory subpopulation"
                         )
)


functions <- data.frame("functions"= 
                          c("$E(t)$",
                            "$I(t)$",
                            "$\\mathcal{S}_e(x)$",
                            "$\\mathcal{S}_i(x)$"
                          ),
                        "description" =
                          c("external input to excitatory subpopulation",
                            "external input to inhibitory subpopulation",
                            "sigmoidal response function for excitatory subpopulation",
                            "sigmoidal response functions for inhibitory subpopulation"))


table_cap5 <- tables("5", caption = "Defining the System's Parameters and Functions")
knitr::kables(list(kbl(parameters,col.names = c(" ", " "), booktabs = T,linesep = "", escape = F) %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "hotpink") %>%
                     column_spec(1, color = "white", background = spec_color(mtcars$mpg[1:nrow(parameters)], begin = .2 ,end = 0.7), popover = paste("parameters:", parameters$parameters[1:nrow(parameters)])) %>%
                     add_header_above(c("System Parameters" = 2), align = "l", line = F),
                   
                   kbl(functions ,col.names = c(" ", " "), booktabs = T, linesep = "") %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "darkslategray") %>%
                     column_spec(1, color = "white", background = spec_color(1:nrow(functions), begin=0.5, end = 0.95, option = "A", direction = -1, alpha = 0.8), popover = paste("functions:", functions$functions[1:nrow(functions)]), bold = T) %>%
                     add_header_above(c("System Functions"=2),align = "l", line = F)), caption = "Defining the System's Parameters and Functions") %>%
  kable_styling(c("striped", "condensed"), full_width = F, position = "c", repeat_header_continued = T, latex_options = "striped") %>%
  kable_paper(full_width = F)
table_cap6 <- tables("6", caption = "Defining the System's Parameters and Functions")
parameters <- data.frame("parameters"= 
                           c("$\\bf c_1$",
                             "$\\bf c_2$",
                             "$\\bf c_3$",
                             "$\\bf c_4$",
                             "$\\bf P$", 
                             "$\\bf Q$",
                             "$\\bf k_e$",
                             "$\\bf k_i$",
                             "$\\bf \\tau_e$", 
                             "$\\bf \\tau_i$",
                             "$\\bf r_e$",
                             "$\\bf r_i$",
                             "$\\bf a_e$",
                             "$\\bf a_i$",
                             "$\\bf \\theta_e$",
                             "$\\bf \\theta_i$"),
                         "description" = c(
                           "excitatory coupling constant\n applied to excitatory subpopulation",
                           "inhibitory coupling constant\n applied to excitatory subpopulation",
                           "excitatory coupling constant\n applied to inhibitory subpopulation",
                           "inhibitory coupling constant\n applied to inhibitory subpopulation",
                           "external input to excitatory cells",
                           "external input to inhibitory cells",
                           "non-dimensional constant for $\\mathcal{S}_e \\lt 1$",
                           "non-dimensional constant for $\\mathcal{S}_i \\lt 1$",
                           "time constant",
                           "time constant",
                           "refractory delay for excitatory subpopulation",
                           "refractory delay for inhibitory subpopulation",
                           "describes value of max slope for excitatory subpopulation response function",
                           "describes value of max slope for inhibitory subpopulation response function",
                           "describes location of max slope for excitatory subpopulation response function",
                           "describes location of max slope for inhibitory subpopulation response function"
                         )
)

rows <- seq_len(nrow(parameters) %/% 2)

y <- data.frame(parameters[rows, 1:2], parameters[rows+length(rows), 1:2])

functions <- data.frame("functions"= 
                          c("$\\bf \\mathcal{S}(x)$",
                            "$\\bf E(t)$",
                            "$\\bf I(t)$",
                            "$\\bf \\mathcal{S}_e(x)$",
                            "$\\bf \\mathcal{S}_i(x)$"
                          ),
                        "description" =
                          c("sigmoidal subpopulation response function",
                            "external input to excitatory subpopulation",
                            "external input to inhibitory subpopulation",
                            "sigmoidal response function for excitatory subpopulation",
                            "sigmoidal response functions for inhibitory subpopulation"))

knitr::kables(list(kbl(y,col.names = c(" ", " "," ", " "), booktabs = T,linesep = "", escape = F) %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "hotpink") %>%
                     column_spec(c(1,3), color = "white", background = spec_color(mtcars$mpg[1:nrow(y)], begin = .2 ,end = 0.7), popover = paste("parameters:", parameters$parameters[1:nrow(y)])) %>%
                     add_header_above(c("System Parameters" = 4), align = "l", line = F),
                   
                   kbl(functions ,col.names = c(" ", " "), booktabs = T, linesep = "") %>%
                     kable_styling(full_width = FALSE, font_size = 10, wraptable_width = 1) %>% 
                     row_spec(0, color = "darkslategray") %>%
                     column_spec(1, color = "white", background = spec_color(1:nrow(functions), begin=0.5, end = 0.95, option = "A", direction = -1, alpha = 0.8), popover = paste("functions:", functions$functions[1:nrow(functions)]), bold = T) %>%
                     add_header_above(c("System Functions"=2),align = "l", line = F)), caption = "Defining the System's Parameters and Functions") %>%
  kable_styling(c("striped", "condensed"), full_width = F, position = "c", repeat_header_continued = T, latex_options = "striped") %>%
  kable_paper(full_width = F)
WilsonCowan <- function(t, y, parameters) {
  # couplings
  c1 = 12
  c2 = 4
  c3 = 13
  c4 = 11
  
  # Refractory periods
  rE = 1
  rI = 1
  
  # external inputs
  P = 0
  Q = 0
  
  ki=0.98
  ke=0.97
  I <- y[1]
  E <- y[2]
  dy <- c(
    -I + (ki - rI * I) * si(c3 * E - c4 * I + Q),
    -E + (ke - rE * E) * se(c1 * E - c2 * I + P))
  list(dy)
}
body_cap5 <- fig_nums(name = "phase5", caption = "Flow Field and Nullclines for the Wilson-Cowan System of Equations. Parameters:$c_1=12$, $c_2 = 4$, $c_3= 13$, $c_4=11$, $a_e=1.2$, $\\theta_e=2.8$, $a_i= 1$, $\\theta_i = 4$, $r_e =1$, $r_i=1$, $P=0$, and $Q=0$.")
example4_flowField  <- flowField(WilsonCowan,
                                 xlim = c(-0.01, .5),
                                 ylim = c(-0.01, .5),
                                 add  = FALSE,
                                 ylab = TeX("$E$"),
                                 xlab= TeX("$I$"),
                                 frac=1.2,
                                 col="black")
example4_flowField  <- flowField(WilsonCowan,
                                 xlim = c(-0.0, .5),
                                 ylim = c(-0.0, .5),
                                 add  = FALSE,
                                 ylab = TeX("$E$"),
                                 xlab= TeX("$I$"),
                                 frac=1.2)
grid()
example4_nullclines <- nullclines(WilsonCowan,
                                  xlim = c(-0.0, .5),
                                  ylim = c(-0.0, .5),
                                  col=c("lightseagreen","aquamarine4"))
body_cap6 <- fig_nums(name = "phase6", caption = "Plot of the Equilibrium Points (Steady State solutions) satisfying Theorm 1. Dashed lines are isoclines. Parameters:$c_1=12$, $c_2 = 4$, $c_3= 13$, $c_4=11$, $a_e=1.2$, $\\theta_e=2.8$, $a_i= 1$, $\\theta_i = 4$, $r_e =1$, $r_i=1$, $P=0$, and $Q=0$.")
example4_flowField  <- flowField(WilsonCowan,
                                 xlim = c(-0.0, .5),
                                 ylim = c(-0.0, .5),
                                 add  = FALSE,
                                 ylab = TeX("$E$"),
                                 xlab= TeX("$I$"),
                                 frac=1.2)
grid()
example4_nullclines <- nullclines(WilsonCowan,
                                  xlim = c(-0, .5),
                                  ylim = c(-0, .5),
                                  lty = 2, lwd = 2,
                                  col=c("lightseagreen","aquamarine4"))

## Equilibrium Points

add_point <- points(0.02854, 0.06443, pch=16, col="black")
add_point <- points(0.05582,0.1438, pch=16, col="black")
add_point <- points(0.24079,0.45548, pch=16, col="black")

vanDerPol_stability_4 <- findEquilibrium(WilsonCowan, y0 = c(0.5,0.5))
vanDerPol_stability_5 <- findEquilibrium(WilsonCowan, y0 = c(0.1,0.2))
vanDerPol_stability_6 <- findEquilibrium(WilsonCowan, y0 = c(0.049,0.1))
se <- function(x){
  ae=1.5
  theta_e=2.5
  1/(1+exp(-ae*(x-theta_e)))
}

si <- function(x){
  ai=6
  theta_i= 4.3
  1/(1+exp(-ai*(x-theta_i)))
}

WilsonCowan2 <- function(t, y, parameters) {
  # couplings
  c1 = 13
  c2 = 4
  c3 = 22
  c4 = 2
  
  # Refractory periods
  rE = 1
  rI = 1
  
  # external inputs
  P = 0
  Q = 0
  
  ki=0.825
  ke=0.88
  I <- y[1]
  E <- y[2]
  dy <- c(
    -I + (ki - rI * I) * si(c3 * E - c4 * I + Q),
    -E + (ke - rE * E) * se(c1 * E - c2 * I + P))
  list(dy)
}
body_cap7 <- fig_nums(name = "phase7", caption = "Phase plane and isoclines with parameters chosen to give three stable and two unstable steady states. Parameters: $c_1=13$, $c_2=4$, $c_3=22$, $c_4=2$, $a_e=1.5$, $\\theta_e=2.5$, $a_i=6$, $\\theta_i=4.3$, $r_e=1$, and $r_i=1$.")
example4_flowField  <- flowField(WilsonCowan2,
                                 xlim = c(-0.01, .425),
                                 ylim = c(0, .45),
                                 add  = FALSE,
                                 ylab = TeX("$E,I$"),
                                 xlab= "t",
                                 frac=1)
grid()
example4_nullclines <- nullclines(WilsonCowan2,
                                  xlim = c(-0.01, .425),
                                  ylim = c(0, .45),
                                  lty = 1, lwd = 2,
                                  col=c("lightseagreen","aquamarine4"))
grid()

points(0, 0.04372, pch=16, main="Stable node")
text(0.04, 0.04372, "Stable Node", cex=0.7)

points(0, 0.07823, pch=16, main="Saddle")
text(0.04, 0.07823, "Saddle Point", cex = 0.7)

points(0.17845, 0.20437, pch=16, main="Stable Focus")
text(0.22, 0.20437, "Stable Focus", cex = 0.7)

points(0.4125, 0.3947, pch=16, main="Stable Node")
text(0.37, 0.3947, "Stable Node", cex = 0.7)

points(0.4125, 0.36408, pch=16, main="Saddle")
text(0.37, 0.36408, "Saddle Point", cex = 0.7)


se <- function(x){
  ae=1.3
  theta_e=4
  1/(1+exp(-ae*(x-theta_e)))
}

si <- function(x){
  ai=2
  theta_i= 3.7
  1/(1+exp(-ai*(x-theta_i)))
}

WilsonCowan2 <- function(t, y, parameters) {
  # couplings
  c1 = 16
  c2 = 12
  c3 = 15
  c4 = 3
  
  # Refractory periods
  rE = 1
  rI = 1
  
  # external inputs
  P = 1.25
  Q = 0
  
  ki=0.825
  ke=0.88
  I <- y[1]
  E <- y[2]
  dy <- c(
    -I + (ki - rI * I) * si(c3 * E - c4 * I + Q),
    -E + (ke - rE * E) * se(c1 * E - c2 * I + P))
  list(dy)
}
body_cap8 <- fig_nums(name = "phase8", caption = "Phase Plane Analysis. Determine the steady-state solution by the nullclines' intersection. Parameters: $c_1=16$, $c_2 = 12$, $c_3=15$, $c_4=3$, $a_e = 1.3$, $\\theta_e=4$, $a_i=2$, $\\theta_i = 3.7$, $r_e=1$, $r_i=1$.")
example4_flowField  <- flowField(WilsonCowan2,
                                 xlim = c(-0.01, .425),
                                 ylim = c(0, .5),
                                 add  = FALSE,
                                 ylab = TeX("$E$"),
                                 xlab= TeX("$I$"),
                                 frac=1)
grid()
example4_nullclines <- nullclines(WilsonCowan2,
                                  xlim = c(-0.01, .425),
                                  ylim = c(0, .5),
                                  lty = 2, lwd = 2,
                                  col=c("lightseagreen","aquamarine4"))

body_cap9 <- fig_nums(name = "phase9", caption = "Phase Plane Analysis showing limit cycle trajectory in response to constant simulation $P=1.25$. Dashed lines are nullclines. Parameters: $c_1=16$, $c_2 = 12$, $c_3=15$, $c_4=3$, $a_e = 1.3$, $\\theta_e=4$, $a_i=2$, $\\theta_i = 3.7$, $r_e=1$, $r_i=1$.")
example4_flowField  <- flowField(WilsonCowan2,
                                 xlim = c(-0.01, .425),
                                 ylim = c(0, .5),
                                 add  = FALSE,
                                 ylab = TeX("$E$"),
                                 xlab= TeX("$I$"),
                                 frac=1)
grid()
example4_nullclines <- nullclines(WilsonCowan2,
                                  xlim = c(-0.01, .425),
                                  ylim = c(0, .5),
                                  lty = 2, lwd = 2,
                                  col=c("lightseagreen","aquamarine4"))

y0 <- matrix(c(0.1,0.3,
               0,0,
               0.1,0.2), 3, 2, byrow = TRUE)
example4_trajectory <- trajectory(WilsonCowan2,
                                  y0   = y0,
                                  pch=16,
                                  tlim = c(0, 100),
                                  col="black",
                                  add=T, ylab=TeX("$E, I$"), xlab=TeX("$t$"))
grid()
body_cap10 <- fig_nums(name = "phase10", caption = "$I(t)$ and $E(t)$ for limit cycle shown in Fig. 9. The limit cycle depends on the value of $P$, i.e. $Q$ being set equal to zero.")

example4_solution <- numericalSolution(WilsonCowan2,
                                       y0 = c(0.0, 0.1),
                                       type = "two",
                                       col=c("cornflowerblue", "aquamarine4"),
                                       add.legend = T,
                                       xlab = TeX("$t$"),
                                       ylab = c(TeX("$I$"), TeX("$E$")),
                                       add.grid = F,
                                       tlim = c(0,20),
                                       lwd=2,
                                       ylim=c(0,0.3),
                                       xlim=c(0,18))
