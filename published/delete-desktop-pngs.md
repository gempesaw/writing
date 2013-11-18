A huge part of doing QA is taking screenshots and videos. Luckily, OS X makes it super easy to take screenshots with `Command + Shift + 4`. Unluckily, doing that all day long every day means the desktop gets super cluttered with your beautiful bug report pngs! Instead of manually deleting 'em, let Emacs do it for you :)

    (defun delete-all-pngs-on-desktop ()
      "Opens a dired to desktop, marks all pngs, and tries to delete
    them"
      (interactive)
      (save-window-excursion
        (dired "~/Desktop")
        (revert-buffer)
        (dired-mark-files-regexp "png" nil)
        (dired-do-delete)))

Go to the desktop folder in dired, refresh so it's up to date, mark all the pngs, and delete 'em. Time saved, pew pew!
