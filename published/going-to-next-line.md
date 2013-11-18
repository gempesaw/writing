Writing perl and php, I often find myself needing commas or semicolons at the end of a line. After I got tired of doing `C-e ; <return>` and `C-e , <return>` all the time I decided I'd make things easier on myself.

    (global-set-key (kbd "C-,") (lambda () (interactive)
                                  (end-of-line)
                                  (insert ",")
                                  (indent-according-to-mode)
                                  (forward-line 1)
                                  (indent-according-to-mode)))

    (global-set-key (kbd "C-;") (lambda () (interactive)
                                  (end-of-line)
                                  (insert ";")
                                  (indent-according-to-mode)
                                  (forward-line 1)
                                  (indent-according-to-mode)))

I bound `C-,` and `C-;` to get to the end of the line, insert the appropriate character, indent, go down a single line, and indent the new line, too. I find them to be useful because it allows me to finish up a line of code from anywhere on the line. I also did something similar on `C-<return>`:


    (global-set-key (kbd "C-<return>") (lambda () (interactive)
                                         (end-of-line)
                                         (reindent-then-newline-and-indent)))

Instead of inserting any punctuation, this just opens up a new line beneath the current one with the proper identation and moves the cursor there. These are pretty small little things but I use them multiple times a day to save me numerous keystrokes.
