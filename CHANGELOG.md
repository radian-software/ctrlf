# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## Unreleased
### Features
* More Isearch compatibility was added in [#48]. Additions include:
  `ctrlf-occur`, `ctrlf-forward-symbol`,
  `ctrlf-forward-symbol-at-point`, `ctrlf-toggle-regexp`,
  `ctrlf-toggle-symbol`, and matching Isearch bindings for those.
* Autoload `ctrlf-forward-fuzzy`, `ctrlf-forward-fuzzy-regexp`,
  `ctrlf-backward-fuzzy`, and `ctrlf-backward-fuzzy-regexp` ([#50]).
* We bind `C-s`, `C-r`, `C-M-s`, and `C-M-r` directly instead of using
  `remap`, which means that bindings from unrelated packages may not
  conflict with them. If you remapped the Isearch commands in your
  configuration, this means you will need to update
  `ctrlf-mode-bindings`. See [#51].
* It is now possible to disable `ctrlf-mode` buffer-locally by means
  of `ctrlf-local-mode` ([#52], [#53]).
* Previously, bindings would not work correctly during a CTRLF search
  if it was performed immediately after Emacs started up, before using
  any other command that uses the minibuffer. This has now been fixed
  ([#55]).

### Enhancements
* Compatibility with evil-mode's jump list and search history features
  was included in [#59]. This integration happens automatically and
  requires no user input/customisation.

[#48]: https://github.com/raxod502/ctrlf/issues/48
[#50]: https://github.com/raxod502/ctrlf/pull/50
[#51]: https://github.com/raxod502/ctrlf/issues/51
[#52]: https://github.com/raxod502/ctrlf/issues/52
[#53]: https://github.com/raxod502/ctrlf/pull/53
[#55]: https://github.com/raxod502/ctrlf/issues/55
[#59]: https://github.com/raxod502/ctrlf/issues/59

## 1.1 (released 2020-07-16)
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
