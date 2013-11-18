Sometimes I've got my window layout set up just right, and then a command like `tex-file` stomps all over my hard work. `winner-mode` helps with one off cases, but `tex-file` always pops up a new window for the buffer containing the tex compilation. I usually don't care about the compilation, especially since it usually doesn't fail, so let's stop popping up that window:

    (add-hook 'latex-mode-hook
              (lambda ()
                (define-key latex-mode-map (kbd "C-c C-f")
                  (lambda ()
                    (interactive)
                    (save-buffer)
                    (save-window-excursion
                      (tex-file))))))

`tex-file` is usually at `C-c C-f`, so let's replace it in the key map by my interactive function. It saves the buffer and uses `save-window-excursion` to keep everything in the same place. Hooray :)
