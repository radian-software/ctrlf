# CTRLF

CTRLF (pronounced "control F") is an intuitive and efficient solution
for single-buffer text search in Emacs, replacing packages such as:

* [Isearch](https://www.gnu.org/software/emacs/manual/html_node/emacs/Incremental-Search.html)
* [`M-x occur`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Other-Repeating-Search.html)
* [Swiper](https://github.com/abo-abo/swiper)
* [`avy`](https://github.com/abo-abo/avy)
* [helm-swoop](https://github.com/emacsorphanage/helm-swoop)

Documentation will come when the basic features have been implemented.
Usage is as follows. First install with
[`straight.el`](https://github.com/raxod502/straight.el):

    (straight-use-package
      '(ctrlf :host github :repo "raxod502/ctrlf"))

Then turn on `ctrlf-mode` to remap the bindings of Isearch (by default
`C-s`, `C-r`, `C-M-s`, `C-M-r`, `M-s w`, `M-s _`, `M-s .`) to use
CTRLF instead of Isearch.
