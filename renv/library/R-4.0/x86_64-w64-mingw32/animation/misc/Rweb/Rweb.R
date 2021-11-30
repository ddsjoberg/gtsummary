## we can generate animations using Rweb in real time

## very nasty tricks because the server has no animation package
source('http://gitorious.org/yihui/animation/blobs/raw/master/R/ani.options.R')
source('http://gitorious.org/yihui/animation/blobs/raw/master/R/zzz.R')
.onLoad()
## source('http://gitorious.org/yihui/animation/blobs/raw/master/R/ani.pause.R')
## source('http://gitorious.org/yihui/animation/blobs/raw/master/R/ani.record.R')
saveHTML = function(expr, img.name = 'Rplot',
                    global.opts = '', single.opts = '', ...) {
    oopt = ani.options(...)

    ## replace special chars first: http://api.jquery.com/category/selectors/
    spec.chars = strsplit("!\"#$%&'()*+,./:;?@[\\]^`{|}~", '')[[1]]
    img.name0 = strsplit(img.name, '')[[1]]
    img.name0[img.name0 %in% spec.chars] = '_'
    img.name0 = paste(img.name0, collapse = '')

    ## deparse the expression and form the verbose description
    .dexpr = NULL
    if (isTRUE(ani.options('verbose'))) {
        info = sessionInfo()
        .dexpr = deparse(substitute(expr))
        if (length(.dexpr) >=3 && .dexpr[1] == '{' && tail(.dexpr, 1) == '}') {
            .dexpr = sub('^[ ]{4}', '', .dexpr[-c(1, length(.dexpr))])
        }
        .dexpr = append(.dexpr, c(strwrap(paste(ani.options('description'),
                                                collapse = ' '),
                        width = ani.options('ani.width')/8, exdent = 2,
                        prefix = '## '),
                               sprintf('library(%s)', names(info$otherPkgs))), 0)
        ## append sessionInfo()
        .dexpr = append(.dexpr, paste('##', c(R.version.string,
                                      paste('Platform:', info$platform),
                 strwrap(paste('Other packages:',
                               paste(sapply(info$otherPkgs, function(x)
                        paste(x$Package, x$Version)), collapse = ', '))))))
        .dexpr = paste('	<div class="scianimator" style="width: ',
                       ani.options('ani.width'), 'px;"><pre class="brush: r">',
                       paste(.dexpr, collapse = '\n'), '</pre></div>', sep = '')
    }

    ani.type = ani.options('ani.type')
    ani.dev = ani.options('ani.dev')
    if (is.character(ani.dev)) ani.dev = get(ani.dev)
    imgdir = file.path(ani.options('outdir'), ani.options('imgdir'))
    dir.create(imgdir, showWarnings = FALSE, recursive = TRUE)

    img.fmt = file.path(imgdir, paste(img.name, '%d', '.', ani.type, sep = ''))
    ani.options(img.fmt = img.fmt)
    if ((use.dev <- ani.options('use.dev')))
        ani.dev(img.fmt,
            width = ani.options('ani.width'), height = ani.options('ani.height'))
    eval(expr)
    if (use.dev) dev.off()

    html = readLines('http://gitorious.org/yihui/animation/blobs/raw/master/inst/misc/Rweb/index.html')
    n = grep('<!-- highlight R code -->', html, fixed = TRUE)

    div.str = sprintf('	<div class="scianimator"><div id="%s" style="display: inline-block;"></div></div>', img.name0)

    n = grep('<!-- highlight R code -->', html, fixed = TRUE)
    html = append(html, c(div.str, .dexpr), n - 1)

    js.temp = readLines('http://gitorious.org/yihui/animation/blobs/raw/master/inst/misc/scianimator/js/template.js')
    if (!ani.options('autoplay')) js.temp = js.temp[-10]
    js.temp = paste(js.temp, collapse = '\n')
    imglen = length(list.files(imgdir, pattern = paste(img.name, '[0-9]+\\.', ani.type, sep = '')))
    imglist = paste('http://data-engine.tama.ac.jp/cgi-bin/Rweb/nph-imageDump.pl?',
    file.path(imgdir, sprintf(paste(img.name, '%d.', ani.type, sep = ''),
                              seq_len(imglen))), sep = '')
    js.temp = sprintf(js.temp, global.opts, img.name0,
                      paste(shQuote(imglist, 'sh'), collapse = ', '),
                      ani.options('ani.width'),
                      1000 * ani.options('interval'),
                      ifelse(ani.options('loop'), 'loop', 'none'),
                      ifelse(nzchar(single.opts), paste(',\n', single.opts), ''),
                      img.name0
    )
    js.temp = c('<script type="text/javascript">', js.temp, '</script>')
    html = c(html, js.temp)
    cat('</pre>', html, '<pre>', sep = '\n', file = '')

    ani.options(oopt)

}
