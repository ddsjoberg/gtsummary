## -----------------------------------------------------------------------------
library(htmlTable)
library(magrittr)

setHtmlTableTheme(theme = "Google docs")

output <- 
  matrix(paste("Content", LETTERS[1:16]), 
         ncol = 4, byrow = TRUE)

output %>% 
  htmlTable(header =  paste(c("1st", "2nd", "3rd", "4th"), "header"),
            rnames = paste(c("1st", "2nd", "3rd", "4th"), "row"),
            rgroup = c("Group A", "Group B"),
            n.rgroup = c(2, 2),
            cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
            n.cgroup = c(2, 2), 
            caption = "Basic table with both column spanners (groups) and row groups",
            tfoot = "&dagger; A table footer commment")

## -----------------------------------------------------------------------------
setHtmlTableTheme(pos.caption = "bottom")

output %>% 
  addHtmlTableStyle(css.rgroup = "font-style: italic") %>%
  htmlTable(header =  paste(c("1st", "2nd", "3rd", "4th"), "header"),
            rnames = paste(c("1st", "2nd", "3rd", "4th"), "row"),
            rgroup = c("Group A", "Group B", ""),
            n.rgroup = c(1, 2),
            cgroup = c("Cgroup 1", "Cgroup 2&dagger;"),
            n.cgroup = 3, 
            caption = "A slightly differnt table with a bottom caption",
            tfoot = "&dagger; A table footer commment")

## ---- results='markup', message=FALSE, warning=FALSE--------------------------
data(SCB)

# The SCB has three other coulmns and one value column
library(reshape)
SCB$region <- relevel(SCB$region, "Sweden")
SCB <- cast(SCB, year ~ region + sex, value = "values")

# Set rownames to be year
rownames(SCB) <- SCB$year
SCB$year <- NULL

# The dataset now has the rows
names(SCB)
# and the dimensions
dim(SCB)

## -----------------------------------------------------------------------------
mx <- NULL
for (n in names(SCB)) {
  tmp <- paste0("Sweden_", strsplit(n, "_")[[1]][2])
  mx <- cbind(mx,
              cbind(SCB[[n]], 
                    SCB[[n]] - SCB[[n]][1],
                    SCB[[n]] - SCB[[tmp]]))
}
rownames(mx) <- rownames(SCB)
colnames(mx) <- rep(c("Age", 
                      "&Delta;<sub>int</sub>",
                      "&Delta;<sub>std</sub>"), 
                    times = ncol(SCB))
mx <- mx[,c(-3, -6)]

# This automated generation of cgroup elements is 
# somewhat of an overkill
cgroup <- 
  unique(sapply(names(SCB), 
                function(x) strsplit(x, "_")[[1]][1], 
                USE.NAMES = FALSE))
n.cgroup <- 
  sapply(cgroup, 
         function(x) sum(grepl(paste0("^", x), names(SCB))), 
         USE.NAMES = FALSE)*3
n.cgroup[cgroup == "Sweden"] <-
  n.cgroup[cgroup == "Sweden"] - 2

cgroup <- 
  rbind(c(cgroup, rep(NA, ncol(SCB) - length(cgroup))),
        Hmisc::capitalize(
          sapply(names(SCB), 
                 function(x) strsplit(x, "_")[[1]][2],
                 USE.NAMES = FALSE)))
n.cgroup <- 
  rbind(c(n.cgroup, rep(NA, ncol(SCB) - length(n.cgroup))),
        c(2,2, rep(3, ncol(cgroup) - 2)))

print(cgroup)
print(n.cgroup)

## -----------------------------------------------------------------------------
htmlTable(txtRound(mx, 1), 
          cgroup = cgroup,
          n.cgroup = n.cgroup,
          rgroup = c("First period", 
                     "Second period",
                     "Third period"),
          n.rgroup = rep(5, 3),
          tfoot = txtMergeLines("&Delta;<sub>int</sub> correspnds to the change since start",
                                "&Delta;<sub>std</sub> corresponds to the change compared to national average"))

## -----------------------------------------------------------------------------
mx %>% 
  txtRound(digits = 1) %>% 
  addHtmlTableStyle(align = "rrrr|r",
                    spacer.celltype = "double_cell") %>% 
  htmlTable(cgroup = cgroup,
            n.cgroup = n.cgroup,
            rgroup = c("First period", 
                       "Second period",
                       "Third period"),
            n.rgroup = rep(5, 3),
            tfoot = txtMergeLines("&Delta;<sub>int</sub> correspnds to the change since start",
                                  "&Delta;<sub>std</sub> corresponds to the change compared to national average"))

## -----------------------------------------------------------------------------
mx %>% 
  txtRound(digits = 1) %>% 
  addHtmlTableStyle(align = "rrrr|r",
                    align.header = "c",
                    col.columns = c(rep("#E6E6F0", 4),
                          rep("none", ncol(mx) - 4))) %>% 
  htmlTable(cgroup = cgroup,
            n.cgroup = n.cgroup,
            rgroup = c("First period", 
                       "Second period",
                       "Third period"),
            n.rgroup = rep(5, 3),
            tfoot = txtMergeLines("&Delta;<sub>int</sub> correspnds to the change since start",
                                  "&Delta;<sub>std</sub> corresponds to the change compared to national average"))

## -----------------------------------------------------------------------------
mx %>% 
  txtRound(digits = 1) %>% 
  addHtmlTableStyle(align = "rrrr|r",
                    align.header = "c",
                    col.columns = c(rep("#E6E6F0", 4),
                          rep("none", ncol(mx) - 4)),
                    col.rgroup = c("none", "#FFFFCC")) %>% 
  htmlTable(cgroup = cgroup,
            n.cgroup = n.cgroup,
            # I use the &nbsp; - the no breaking space as I don't want to have a
            # row break in the row group. This adds a little space in the table
            # when used together with the cspan.rgroup=1.
            rgroup = c("1st&nbsp;period", 
                       "2nd&nbsp;period",
                       "3rd&nbsp;period"),
            n.rgroup = rep(5, 3),
            tfoot = txtMergeLines("&Delta;<sub>int</sub> correspnds to the change since start",
                                  "&Delta;<sub>std</sub> corresponds to the change compared to national average"),
            cspan.rgroup = 1)

## -----------------------------------------------------------------------------
cols_2_clr <- grep("&Delta;<sub>std</sub>", colnames(mx))
# We need a copy as the formatting causes the matrix to loos
# its numerical property
out_mx <- txtRound(mx, 1)

min_delta <- min(mx[,cols_2_clr])
span_delta <- max(mx[,cols_2_clr]) - min(mx[,cols_2_clr]) 
for (col in cols_2_clr) {
  out_mx[, col] <- mapply(function(val, strength)
    paste0("<span style='font-weight: 900; color: ", 
           colorRampPalette(c("#009900", "#000000", "#990033"))(101)[strength],
           "'>",
           val, "</span>"), 
    val = out_mx[,col], 
    strength = round((mx[,col] - min_delta)/span_delta*100 + 1),
    USE.NAMES = FALSE)
}

out_mx %>% 
  addHtmlTableStyle(align = "rrrr|r",
                    align.header = "cccc|c",
                    pos.rowlabel = "bottom", 
                    col.rgroup = c("none", "#FFFFCC"),
                    col.columns = c(rep("#EFEFF0", 4),
                                    rep("none", ncol(mx) - 4))) %>% 
  htmlTable(caption = "Average age in Sweden counties over a period of
                     15 years. The Norbotten county is typically known
                     for having a negative migration pattern compared to
                     Stockholm, while Uppsala has a proportionally large 
                     population of students.",
            rowlabel = "Year",
            cgroup = cgroup,
            n.cgroup = n.cgroup,
            rgroup = c("1st&nbsp;period", 
                       "2nd&nbsp;period",
                       "3rd&nbsp;period"),
            n.rgroup = rep(5, 3),
            tfoot = txtMergeLines("&Delta;<sub>int</sub> corresponds to the change since start",
                                  "&Delta;<sub>std</sub> corresponds to the change compared to national average"),
            cspan.rgroup = 1)

