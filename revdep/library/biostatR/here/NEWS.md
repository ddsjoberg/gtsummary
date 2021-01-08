# here 1.0.1 (2020-12-13)

- Adapt to testthat 3.0.1.


# here 1.0.0 (2020-11-15)

## Features

- New `here::i_am()` offers a new recommended way to declare the project root. Instead of relying on special files or directories that indicate the project root, each script and report now can declare its own location relative to the project root (#27).
- `dr_here()` shows the initial and current working directory (#18, #55).

## Documentation

- Improve readme and add "Getting started" vignette (#44, @sharlagelfand). Extend "Getting started" vignette with a detailed description of the behavior and use cases (#55).
- Update `?here` to address conflict with `lubridate::here()` (#37, @nzgwynn).
- New "rmarkdown" vignette (#55).
- `set_here()` is superseded in favor of `here::i_am()` (#61).
- More explicit documentation for the `...` argument to `here()` (#7).

## Internal

- Add testthat tests (#57).
- Use GitHub Actions for checks (#52).
- Requires rprojroot >= 2.0.1.
- Re-license as MIT (#50).
- Added Jennifer Bryan as contributor (#32).


# here 0.1 (2017-01-25)

Initial CRAN release.

- Main function `here()`
    - Uses a fixed root that contains a `.here` file or can be either an RStudio project, an R package, a `remake` project, a Projectile project, or a VCS repository.
    - The root is established at package loading time, and shown with a message.
    - The current working directory is used as a fallback.
- The `set_here()` function creates a `.here` file so that a directory is recognized as root by `here()`.
- The `dr_here()` function explains the reasoning of `here()` for the current session.
