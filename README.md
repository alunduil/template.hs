<!-- vale RedHat.Headings = NO -->
# template.hs
<!-- vale RedHat.Headings = YES -->

[Homepage][repository]

By Alex Brandt <alunduil@gmail.com>

## Description

You can use template.hs to create a new GitHub repository. The new repository
has Haskell, Visual Studio Code Dev Containers, and various GitHub actions
ready to use.

## Terms of use

You are free to use template.hs as a basis for your own projects without any
conditions. See the [LICENSE] file for details.

## Prerequisites

1. Visual Studio Code with "Remote Development" installed

## How to use this template

1. Visit [the repository][repository]
1. Click "Use this template"
1. Follow the GitHub Docs to [Create a repo][create a repo]
1. Open Visual Studio Code
1. Open the shell prompt (Ctrl+shift+p)
1. Type "clone repository in container" and press return
1. Input the GitHub URL of your new repository
1. In the resulting terminal (Ctrl+\`), run: `cabal run initialise`
1. Browse to "Setting" -> "Actions" -> "General"
1. Tick "Allow GitHub Actions to create and approve pull requests"
1. Continue working on your awesome project

## Documentation

* [LICENSE]: The license governing use of template.hs

## Getting help

* [GitHub Issues][issues]: Support requests, bug reports, and feature requests

## How to help

* Submit [issues] for problems or questions
* Submit [pull requests] for proposed changes

[create a repo]: https://docs.github.com/en/get-started/quickstart/create-a-repo
[issues]: https://github.com/alunduil/template.hs/issues
[LICENSE]: ./LICENSE
[pull requests]: https://github.com/alunduil/template.hs/pulls
[repository]: https://github.com/alunduil/template.hs
