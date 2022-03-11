# ob-bitfield

Generate bitfield diagrams using the Python [bitfield](https://github.com/Arth-ur/bitfield) package and [org-babel](https://orgmode.org/worg/org-contrib/babel/).

## Installation

Install the [bitfield](https://github.com/Arth-ur/bitfield) Python package:
```
pip install bit_field json5
```

[Imagemagick](https://imagemagick.org/) is also required if you would like to generate images other than SVGs. On Debian/Ubuntu this can be installed with `sudo apt install imagemagick`, and on macOS it can be installed with `brew install imagemagick`.

Download [ob-bitfield.el](https://raw.githubusercontent.com/gsingh93/ob-bitfield/main/ob-bitfield.el) to `~/.emacs.d` or another directory in your [`load-path`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html). You can then enable the package in one of the following ways.

1. Call [`org-babel-do-load-languages`](https://orgmode.org/manual/Languages.html):

```elisp
(org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (bitfield . t)
     (ditaa . t)))
```

2. Add `(require 'ob-bitfield)` to your Emacs init file
3. If using `use-package`, you can add this to your init file:
```elisp
(use-package ob-bitfield
  :after org)
```

## Usage

Create a `bitfield` source block, for example:
```org
#+begin_src bitfield :lanes 2 :file out.png
[
    { "name": "IPO",   "bits": 8, "attr": "RO" },
    {                  "bits": 7 },
    { "name": "BRK",   "bits": 5, "attr": "RW", "type": 4 },
    { "name": "CPK",   "bits": 1 },
    { "name": "Clear", "bits": 3 },
    { "bits": 8 }
]
#+end_src
```

Use `org-babel-execute-src-block` (`C-c C-c`) to execute the source block. The results will look as follows:
```org
#+RESULTS:
[[file:out.png]]
```

You can view the image inline with `org-toggle-inline-images` (`C-c C-x C-v`).

If `:file` does not end in the ".svg" extension, then Imagemagick is used to convert it to the specified file type.

### Header Arguments

By default, `:results` is set to `file graphics` and `:exports` is set to `results`.

`:file` is the only required header argument.

Most arguments from the [bitfield](https://github.com/Arth-ur/bitfield) package are supported. If an argument is not specified, the default value from the package is used. The full list of supported arguments is:

- `:lanes`
- `:vspace`
- `:hspace`
- `:bits`
- `:fontfamily`
- `:fontweight`
- `:fontsize`
- `:compact`
