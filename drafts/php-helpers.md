I finally got around to making some tiny customizations my Emacs PHP editing environment, where I'm just using `php-mode` off of marmalade and Flycheck, and ... not much else. First, `php-mode` wants `<TAB>` for `indent-for-tab-command`, but I don't want it there. I put `smart-tab` on my `<TAB>` key, so I need to take it back within the mode-map.

    (define-key php-mode-map (kbd "<tab>") 'smart-tab)

There's a trick about using your own minor-mode-map to globally override bindings, but that won't work here since php-mode is also a minor-mode. So, we have to change it in `php-mode-map` itself.

Secondly, there's a cool (surprisingly simple!) function called `php-send-region` which takes the current region as input and sends it to a `call-process`. By default, it's on `C-c C-r`, but I'm not really interested in marking a region every time I want to eval, especially when I'll probably need to mark most, if not all, of the entire buffer. So, we can use a simple function to pass it the whole buffer every time and replace it with that:

    (defun php-send-buffer ()
      (interactive)
      (with-current-buffer "*PHP*" (erase-buffer))
      (php-send-region (point-min) (point-max)))

    (define-key php-mode-map (kbd "C-c C-r") 'php-send-buffer)

`php-send-region` sends all of its output to the `*PHP*` buffer, which is fine, except after a couple evals it gets confusing keeping track of which one was the most recent. This is especially a problem for me when I was using the SimpleTest library, which seems to scramble the output order a little bit. No biggie - just `erase-buffer` the `*PHP*` buffer before we send it the new stuff, and we're good to go.

We can also do another similarly simple function to mimic the familiar elisp `C-x C-e` functionality:

    (defun php-send-line ()
      (interactive)
      (with-current-buffer "*PHP*" (erase-buffer))
      (php-send-region (point-at-bol) (point-at-eol)))

    (define-key php-mode-map (kbd "C-x C-e") 'php-send-line)
