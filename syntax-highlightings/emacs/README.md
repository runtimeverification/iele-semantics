# Syntax highlighting of IELE for Emacs
## Installation

Add the code below to your .emacs file:

```clojure
(load "path/to/this/file" )
(add-to-list 'auto-mode-alist '("\\.iele$" . iele-mode)) ;; to launch iele-mode for .iele files
```

## Development

Open `iele.el` in emacs.  
After modifying `iele.el`, `M-x` then run `eval-current-buffer` command.  
Next, open a `.iele` file, `M-x` then run `iele-mode` command to see the update.  