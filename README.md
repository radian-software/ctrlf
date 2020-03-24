# CTRLF

CTRLF (pronounced "control F") is an intuitive and efficient solution
for single-buffer text search in Emacs, replacing packages such as
[Isearch](https://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html),
[Swiper](https://github.com/abo-abo/swiper), and
[helm-swoop](https://github.com/emacsorphanage/helm-swoop).

<!-- toc -->

- [Getting started](#getting-started)
- [User guide](#user-guide)
  * [Search flow](#search-flow)
  * [Customization](#customization)
  * [Search styles](#search-styles)
  * [Miscellaneous](#miscellaneous)
- [Why use CTRLF?](#why-use-ctrlf)
  * [Why not Isearch?](#why-not-isearch)
  * [Why not Swiper?](#why-not-swiper)
  * [Why not helm-swoop?](#why-not-helm-swoop)
  * [Why not `M-x occur`?](#why-not-m-x-occur)
  * [Why not Avy?](#why-not-avy)
- [Contributing](#contributing)

<!-- tocstop -->

## Getting started

CTRLF is not currently listed on MELPA. Therefore, the best way is to
install it is with
[`straight.el`](https://github.com/raxod502/straight.el):

```elisp
(straight-use-package
  '(ctrlf :host github :repo "raxod502/ctrlf"))
```

To enable CTRLF, simply add to your init-file:

```elisp
(ctrlf-mode +1)
```

Note that the autoloading has been configured so that enabling
`ctrlf-mode` will not actually load CTRLF until you use one of its
commands.

Now the usual Isearch bindings will use CTRLF instead:

* `C-s`: `ctrlf-forward-literal` (originally `isearch-forward`)
* `C-r`: `ctrlf-backward-literal` (originally `isearch-backward`)
* `C-M-s`: `ctrlf-forward-regexp` (originally
  `isearch-forward-regexp`)
* `C-M-r`: `ctrlf-backward-regexp` (originally
  `isearch-backward-regexp`)

## User guide

The design philosophy of CTRLF is to:

* replicate the user experience that users expect from text search
  based on widespread implementations of ctrl+F functionality
* re-use the flow and keybindings of Isearch
* emphasize reliability and consistency

As such, if you are familiar with Isearch then you will probably be at
home in CTRLF.

### Search flow

First you must start a search using one of the following keybindings:

* `C-s`: Search forward for a literal string.
* `C-r`: Search backward for a literal string.
* `C-M-s`: Search forward for a regexp.
* `C-M-r`: Search backward for a regexp.

During a search, the following core keybindings are available:

* `C-s`: Move to next match. With prefix argument, also convert a
  regexp search back to a literal search. If there is no search query,
  insert the previous one, as if you had typed `M-p`. This allows you
  to resume a search with `C-s C-s`.
* `C-r`: Move to previous match. With prefix argument, also convert a
  regexp search back to a literal search. If there is no search query,
  insert the previous one, as if you had typed `M-p`. This allows you
  to resume a search with `C-r C-r`.
* `C-M-s`: Move to next match and convert a literal search to a regexp
  search.
* `C-M-r`: Move to previous match and convert a literal search to a
  regexp search.
* `RET`: Finish search, leaving point at the currently selected match.
* `C-g`: Abort search, returning point to its original location.

The following additional keybindings are available, emulating standard
Emacs bindings:

* `M-<`: Move to first match.
* `M->`: Move to last match.
* `C-v`: Move to first match that appears below the currently visible
  part of the buffer. In other words, move down by a page.
* `M-v`: Move to first match that appears above the currently visible
  part of the buffer. In other words, move up by a page.
* `C-l`: Scroll so that the currently selected match is at the center
  of the window. Typing `C-l` multiple successive times, or providing
  a prefix argument, has the same effect as usual (see the docstring
  of the `recenter-top-bottom` command).

CTRLF-specific bindings are located under the `C-o` prefix, because it
is unusual to run the `open-line` command during a text search and
because `o` is mnemonic for "other":

* `C-o c`: Toggle case-sensitive search. By default, search is
  case-sensitive only if your search contains uppercase letters, like
  in Isearch (following the logic in `isearch-no-upper-case-p`).
* `C-o s`: Change the search style, e.g. between literal or regexp.
  Search styles are explained thoroughly later in this documentation.
  Note that this command is not currently compatible with
  [Selectrum](https://github.com/raxod502/selectrum), although it does
  work with the standard `completing-read` framework provided by
  Emacs.

Other than this, keybindings are completely standard. For example, to
delete the last word in your search query, use `M-DEL`, or to retrieve
the previous search query in the minibuffer history, use `M-p`.

It is standard in Emacs for typing `M-n` after entering the minibuffer
to insert a default value into the minibuffer. In CTRLF, this default
value is the symbol at point.

### Customization

You can customize the visual appearance of CTRLF:

* Face `ctrlf-highlight-active` is used to highlight the currently
  selected match.
* Face `ctrlf-highlight-passive` is used to highlight the other
  currently visible matches.
* Face `ctrlf-highlight-line` is used to highlight the entire line on
  which the currently selected match resides, if
  `ctrlf-highlight-current-line` is non-nil (the default).
* If `ctrlf-auto-recenter` is non-nil, then the currently selected
  match is always kept vertically centered in the window, as if you
  typed `C-l` each time you moved to a new match. This feature is
  disabled by default.
* The index of the currently selected match and the total number of
  matches are displayed at the end of the minibuffer. If
  `ctrlf-show-match-count-at-eol` is non-nil (the default), then this
  information is also shown at the end of the current line in the
  buffer being searched, which alleviates the problem of needing to
  look back and forth between the minibuffer and the buffer being
  searched.
* Zero-length matches (for example, in a regexp search for `^$` which
  would identify all blank lines the buffer) are displayed as thin
  vertical rectangles with a solid color since there is no text to
  highlight. The width of these rectangles relative to the width of a
  normal character is defined by `ctrlf-zero-length-match-width`.

You can also customize the keybindings:

* User option `ctrlf-mode-bindings` lists keybindings that are made
  globally available in Emacs when `ctrlf-mode` is enabled.
* User option `ctrlf-minibuffer-bindings` lists keybindings that are
  made available in the minibuffer during a CTRLF search session.

In addition to the functions already bound in
`ctrlf-minibuffer-bindings`, you can choose to bind `ctrlf-next-match`
and `ctrlf-previous-match`. These functions are the same as
`ctrlf-forward` and `ctrlf-backward`, but they do not have the special
features of inserting the previous search, changing to a literal
search, or starting a new search when not already in a search session.

### Search styles

CTRLF implements support for literal and regexp using an extensible
*search style* system. This functionality is configured using the
`ctrlf-style-alist` user option. End users need not touch this option
unless they wish to do advanced customization or are developing a
package which integrates with CTRLF.

The keys of `ctrlf-style-alist` define the available search styles.
These styles appear in two places:

* As options in the `ctrlf-change-search-style` command bound by
  default to `C-o s`.
* As possible values for the `STYLE` argument to the `ctrlf-forward`
  and `ctrlf-backward` functions.

Basically, a search style defines a way to transform the user's search
query into a regexp which can be passed to `search-forward-regexp` or
`search-backward-regexp`. Here are the built-in search styles:

* `literal`: Search for an exact match to the query string, subject to
  case folding (`C-o c`). This is implemented using `regexp-quote`.
* `regexp`: Search for a regexp provided by the user. If the regexp is
  invalid, CTRLF will display an error message in the minibuffer until
  the problem is corrected.
* `fuzzy`: Split the query string on spaces and search for an
  occurrence of all the sub-parts separated by arbitrary text. To
  include a literal space, or more than one, simply add an additional
  space. For example, `foo $42 baz quux` is turned into
  `foo.*\$42.*baz quux`.
* `fuzzy-regexp`: Same as `fuzzy` except that the individual sub-parts
  are interpreted directly as regexps, so that `foo $42 baz quux` is
  turned into `foo.*$42.*baz quux`.

To define a custom search style, you should proceed according to the
following steps:

* Add an entry to `ctrlf-style-alist` with:
    * The name of the search style as a symbol.
    * The string to display in the prompt when using this search
      style (`:prompt`).
    * A function that will take the user input and return a regexp
      (`:translator`).
    * A function that will take the user input and guess whether
      case-folding should be enabled by default.
* Define wrapper functions after the fashion of
  `ctrlf-forward-literal` and `ctrlf-backward-literal` that use
  `ctrlf-forward` and `ctrlf-backward`, respectively, as subroutines.
* Bind these functions in `ctrlf-mode-bindings`.

### Miscellaneous

The minibuffer history for CTRLF is stored in the variable
`ctrlf-search-history`. You can access it during a search session
using `M-p` and `M-n`. Typing `M-n` at the beginning of a session will
perform a search for the symbol at point. Furthermore, typing `C-s` or
`C-r` without any search query is a synonym for `M-p`.

## Why use CTRLF?

This section documents why I decided to write CTRLF instead of using
any of the numerous existing solutions in Emacs.

### Why not Isearch?

Isearch has the right idea for buffer search, but it has serious
usability problems. The main issue, for me, is that it feels extremely
fragile. If you type any command that is not bound in Isearch, then
you exit your search and run that command. This means editing your
search query is awkward and unintuitive. Another issue is
predictability. The behavior of `C-g` is hard to predict because it
depends on not only what you have typed in what order, but also on the
buffer contents and the state of the current search. The handling of
wraparound exacerbates problems of predictability: the number of times
you must type `C-s` to move to the next match is unpredictable and
the minibuffer prompt changes in several different and confusing ways
during this process.

CTRLF takes the basic idea of Isearch, together with most of its
keybindings, but emulates the more reliable user experience of web
browser text search. For example: all editing commands can be used
during a search as usual; `C-g` always has the effect of canceling the
search; and `C-s` always moves to the next candidate, with wraparound
signaled by an overlay which indicates the current match index and the
total number of matches (another UI paradigm borrowed from other
programs).

### Why not Swiper?

The selling point of Swiper is that it shows you an overview of the
matches. Ask yourself: when was the last time you actually got
anything useful out of that overview? Since all the matches are
crammed together, one per line, there is not enough context for
meaningful information to be communicated. Furthermore, Swiper
constrains itself almost exclusively to line-based search by design,
which makes it unsuitable to the task of quick movement within a line
or movement to a commonly occurring search string.

### Why not helm-swoop?

For basically the same reason that Swiper is not a good solution, with
the added complaint that Helm is extraordinarily complex and
in-your-face about this complexity. Single-buffer text search is not a
terribly difficult problem, and given that CTRLF is only about 1,100
lines of code, I think that the 1,700 lines of code of `helm-swoop`
plus the 10,600 that come with Helm is a bit overkill.

### Why not `M-x occur`?

`M-x occur` implements *noninteractive* search, and therefore
unsuitable as a tool for quickly jumping to other parts of the buffer.

### Why not Avy?

It does not usefully support text search outside the currently visible
window.

## Contributing

Please run `make lint` before creating your pull request, otherwise
the CI will fail. If the CI does still fail, check out the results --
it may be that your code doesn't work in one of the older Emacs
versions supported by CTRLF. In that case, run e.g. `make docker
VERSION=25.3` to launch a shell with the appropriate version, at which
point you can re-run `make lint`.

If you change the sections in the README, remember to run `make toc`.
