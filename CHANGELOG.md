# Changelog

All notable changes to this project will be documented in this file.
The format is based on [Keep a Changelog].

## 1.4 (released 2021-12-27)
### Features
* Add a new matching style for matching words ([#61]).
* Add ability to customize default and alternative search styles
  through `ctrlf-default-search-style` and
  `ctrlf-alternate-search-style` ([#83]).
* Keybinding customization is now available via the `ctrlf-mode-map`
  and the `ctrlf-minibuffer-mode-map` keymaps. Existing customization
  via `ctrlf-mode-bindings` and `ctrlf-minibuffer-bindings` are still
  available at the moment to remain backward compatibility but will be
  removed in a future release ([#62]).

### Enhancements
* When `ctrlf-auto-recenter` is enabled, recentering only begins after
  you start typing your search query, not immediately on typing `C-s`.
  This way, if you abort the search immediately, your window is not
  scrolled ([#93]).

### Bugs fixed
* Since version 1.2, CTRLF had a number of bugs related to third-party
  minibuffer bindings. Specifically, it would override `C-s` and `C-r`
  bindings in the minibuffer even when that was totally inappropriate.
  This was due to a side effect of the fix for [#51] (where CTRLF
  would be overridden by global `minibuffer-local-map` bindings
  established by e.g. Helm). We now have a more clever hack that
  solves both problems. The new version restores the previous use of
  `remap` for CTRLF mode bindings, which means your customizations of
  Isearch bindings will again carry over automatically to CTRLF by
  default. Furthermore, CTRLF bindings will override other bindings in
  the minibuffer if and only if you are inside an active CTRLF
  session. Solves [#52], [#67], and [#80].
* Window scroll positions would sometimes be messed up when a buffer
  was visible in multiple windows. This has been fixed ([#81]).
* When a regexp translation function throws an error, we now treat it
  like an invalid regexp rather than CTRLF breaking and not responding
  to further inputs.
* CTRLF previously would crash when moving point over invisible text
  that had a custom `isearch-open-invisible-temporary` property. This
  has been fixed ([#100], [#101]).

[#51]: https://github.com/raxod502/ctrlf/issues/51
[#52]: https://github.com/raxod502/ctrlf/issues/52
[#61]: https://github.com/raxod502/ctrlf/issues/61
[#62]: https://github.com/raxod502/ctrlf/issues/62
[#67]: https://github.com/raxod502/ctrlf/issues/67
[#80]: https://github.com/raxod502/ctrlf/issues/80
[#83]: https://github.com/raxod502/ctrlf/issues/83
[#93]: https://github.com/raxod502/ctrlf/issues/93
[#100]: https://github.com/raxod502/ctrlf/issues/100
[#101]: https://github.com/raxod502/ctrlf/pull/101

## 1.3 (released 2021-02-26)
### Features
* New user option `ctrlf-go-to-end-of-match`, if non-nil, puts point
  at the end of a match when exiting a search, rather than the
  beginning ([#66], [#73])

### Enhancements
* Compatibility with evil-mode's jump list and search history features
  was included in [#59]. This integration happens automatically and
  requires no user input/customisation.

[#59]: https://github.com/raxod502/ctrlf/issues/59
[#66]: https://github.com/raxod502/ctrlf/issues/66
[#73]: https://github.com/raxod502/ctrlf/pull/73

## 1.2 (released 2020-10-20)
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

### Bugs fixed
* Previously, bindings would not work correctly during a CTRLF search
  if it was performed immediately after Emacs started up, before using
  any other command that uses the minibuffer. This has now been fixed
  ([#55]).

[#48]: https://github.com/raxod502/ctrlf/issues/48
[#50]: https://github.com/raxod502/ctrlf/pull/50
[#51]: https://github.com/raxod502/ctrlf/issues/51
[#52]: https://github.com/raxod502/ctrlf/issues/52
[#53]: https://github.com/raxod502/ctrlf/pull/53
[#55]: https://github.com/raxod502/ctrlf/issues/55

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
