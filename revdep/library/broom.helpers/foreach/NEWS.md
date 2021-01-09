## 1.5.1 (2020-10-12)

- Maintainer change (Michelle Wallig; Michelle.Wallig@microsoft.com)

## 1.5.0 (2020-03-29)

- Changed the default value for the `foreachDoparLocal` system option to TRUE.

## 1.4.8 (2020-02-04)

<ul>
<li><p>Added preliminary support for evaluating `%dopar%` expressions in a local environment, when a sequential backend is used. This addresses a long-standing inconsistency in the behaviour of `%dopar%` with parallel and sequential backends, where the latter would evaluate the loop body in the global environment by default. The behaviour of `%dopar%` can be controlled by setting<br/>

`options(foreachDoparLocal=TRUE|FALSE)`<br/>

or equivalently via the system environment variable<br/>

`R_FOREACH_DOPAR_LOCAL=TRUE|FALSE`<br/>

with the R option taking its value from the environment variable. If both are set, the former overrides the latter. The current default value is FALSE, which retains the pre-existing behaviour. It is intended that over time this will be changed to TRUE.<br/>

A side-effect of this change is that the behaviour of `%do%` and `%dopar%` will (eventually) be different for a sequential backend. See https://github.com/RevolutionAnalytics/foreach/issues/3 for more discussion on this issue.</p></li>

<li> Updated tooling used for development. None of these changes should have any effect on package behaviour.
<ul>
<li>Use Roxygen for documentation</li>
<li>Use testthat for unit tests</li>
<li>Convert vignettes to Rmarkdown format (and output HTML instead of PDF)</li>
<li>Convert NEWS file to Markdown format
</ul>
</li>
</ul>

## 1.4.7 (2019-07-27)
- Maintainer change (Hong Ooi; hongooi@microsoft.com).

## 1.4.4 (2017-12-08)
- Changed test report path for compliance with CRAN policies.
- Removed startup message.
- Changed `seq(along=tags)` call in `makeAccum` to `seq(along.with=tags)`; request of Henrik Bengtsson.
- Updated `foreach` help to describe effect of multiple arguments; request of David Winsemius.

## 1.4.3 (2015-10-12)
- Updated maintainer address
    
## 1.4.2 (2014-04-10)
- Unwound circular dependency chain with iterators package.
    
## 1.4.1 (2013-05-29)
- Improved handling of implicitly exported objects, courtesy of Steve Weston.

## 1.4.0 (2012-04-11)
- Removed spurious warning from `getDoSEQ`. Bug report from Ben Barnes.
- Moved welcome message from `.onLoad` to `.onAttach`. Bug report from Benilton Carvalho.
- Modified `setDoPar` and `setDoSeq` to undo changes to .foreachGlobals on error. Bug report from Benilton Carvalho.
- Moved vignettes from `inst/doc` to `vignettes`.
- Modified `DESCRIPTION` file by moving codetools, iterators, and utils from Depends to Imports. Bug report from Suraj Gupta.

## 1.3.5 (2012-03-14)
- Cleanup from previous patch. Bug report from Brian Ripley.

## 1.3.4 (2012-03-12)
- Added support for multiple sequential backends. (Idea and patch from Tyler Pirtle, Matt Furia, and Joseph Hellerstein.)
- Modified `doRUnit.R` to use no more than two cores during R CMD check.

## 1.3.2 (2011-05-08)
- Regularized unit tests so they can run through R CMD check
- Added support for compiler package of 2.13.0 and later.

## 1.3.1 (2010-11-22)
- First R-forge release.

