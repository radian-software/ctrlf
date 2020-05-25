# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Features
* More Isearch compatibility was added in [#48]. Additions include:
  `ctrlf-occur`, `ctrlf-forward-symbol`,
  `ctrlf-forward-symbol-at-point`, `ctrlf-toggle-regexp`,
  `ctrlf-toggle-symbol`, and matching Isearch bindings for those.

### Enhancements
* It is now possible to use the standard Isearch bindings within the
  minibuffer to search the current user input and the minibuffer
  history. Note that this functionality is not compatible with
  Selectrum. See [#41].
* We now disable `blink-matching-paren` automatically when entering
  CTRLF, because (for some reason) it uses `sit-for` to display its
  blink and this of course causes search results to be delayed
  ([#45]).

### Bugs fixed
* CTRLF previously caused an error during Emacs startup unless the
  `map` library was already loaded. This has been fixed ([#49]).
* Several corner cases relating to minibuffer overlays have been fixed
  by improving the generality of the overlay management code ([#46]).

[#41]: https://github.com/raxod502/ctrlf/issues/41
[#45]: https://github.com/raxod502/ctrlf/issues/45
[#46]: https://github.com/raxod502/ctrlf/issues/46
[#48]: https://github.com/raxod502/ctrlf/issues/48
[#49]: https://github.com/raxod502/ctrlf/issues/49

## 1.0 (released 2020-03-31)
### Added
* Package `ctrlf`
* Minor mode `ctrlf-mode`
* Faces:
    * `ctrlf-highlight-active`
    * `ctrlf-highlight-passive`
    * `ctrlf-highlight-line`
* Interface user options:
    * `ctrlf-highlight-current-line`
    * `ctrlf-auto-recenter`
    * `ctrlf-show-match-count-at-eol`
    * `ctrlf-mode-bindings`
    * `ctrlf-minibuffer-bindings`
    * `ctrlf-zero-length-match-width`
* Navigation commands:
    * `ctrlf-next-match`
    * `ctrlf-previous-match`
    * `ctrlf-first-match`
    * `ctrlf-last-match`
    * `ctrlf-next-page`
    * `ctrlf-previous-page`
    * `ctrlf-cancel`
* Utility commands:
    * `ctrlf-change-search-style`
    * `ctrlf-toggle-case-fold-search`
    * `ctrlf-recenter-top-bottom`
* Search commands:
    * `ctrlf-forward-literal`
    * `ctrlf-backward-literal`
    * `ctrlf-forward-regexp`
    * `ctrlf-backward-regexp`
    * `ctrlf-forward-fuzzy`
    * `ctrlf-backward-fuzzy`
    * `ctrlf-forward-fuzzy-regexp`
    * `ctrlf-backward-fuzzy-regexp`
* Low-level user options, variables, and functions:
    * `ctrlf-style-alist`
    * `ctrlf-search-history`
    * `ctrlf-forward`
    * `ctrlf-backward`
* Functions for use in configuration:
    * `ctrlf-split-fuzzy`
    * `ctrlf-translate-fuzzy-literal`
    * `ctrlf-translate-fuzzy-regexp`
    * `ctrlf-no-uppercase-literal-p`
    * `ctrlf-no-uppercase-regexp-p`

[keep a changelog]: https://keepachangelog.com/en/1.0.0/
