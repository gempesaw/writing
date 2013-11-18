    (defun find-function-C-source (fun-or-var &optional file type)
      (save-window-excursion
        (with-help-window (help-buffer)
          (prin1 fun-or-var)
          ;; Use " is " instead of a colon so that
          ;; it is easier to get out the function name using forward-sexp.
          (princ " is ")
          (describe-function-1 fun-or-var)
          (with-current-buffer standard-output
            ;; Return the text we displayed.
            (buffer-string))))
      (cons (help-buffer) 0))

A lot of the functions in Emacs are written in Elisp, and that's super
awesome. Any time I don't understand how a defun works, I can use
`find-function-at-point` and I go to its definition. If a defun is
doing something cool and I want to do a similar thing, its
implementation is just one keystroke away, full of tricks for me to
<strike>shamelessly copy</strike> study and learn from.

However, the core of Emacs is still written in `.c` files, and I
haven't been bothered enough to download them and point my emacs at
the `.c` library. As a result, some attempts to view a function's
definition prompt me for the Emacs source C library. I don't have the
library, and I don't want to keep telling Emacs I don't have it. If I
can't look at the implementation, then I at least want to see the help
documentation for that function, which is available if I invoke
`describe-function` on it. So, instead of me doing the same thing
every time (find-function-at-point, notice it's in a .C file, cancel
out, then describe function), I wanted to skip all my work and have
the defun go straight to describing the function if it's in a .C file.

Tracing down the function call route of `ffap`, there's a defun called
`find-function-C-source` that usually does the work of querying the
user for the C library if necessary. So, I took the part of
`describe-function` that does all the work from `help-fns.el` and
replaced the body of `find-function-C-source`. Thus the code snippet
at the beginning. Now, when I try to find a function thats in a `.C`
file, I see its help documentation instead. If the defun is in a .el
file, I still go to its elisp definition.

The only gotcha was that find-function-search-for-symbol, the caller of
`find-function-C-source`, expects a cons cell to return with a buffer
and point to jump to:

    ( BUFFER . POSITION )

`describe-function` doesn't return that, so we have to do it on our
own. Luckily, we know where we want to go: the beginning of the help
buffer. So, that's what we put as the last line of the function to be
returned.
