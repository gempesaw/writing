A while back, I posted a couple defuns to help with ssh'ing to remote
boxes when you've already configured your `~/.ssh/config` to have the
necessary information about the boxes. It was pretty useful at the
time, but it didn't do anything about fixing the default directory to
help with file autosuggests on the remote box, which are pretty
useful. Recently, I added a single `(cd)` at the end of the defun to
set the `cwd` to the remote user's home directory, but that was
immediately wiped out because `shell` is apparently pretty smart. Any
time I used `cd ~` in the remote shell, Emacs would pick it up and set
the working directory to my local `~/` instead of the remote `~/`. The
same would happen if both the local and the remote directories had the
same absolute filepath - the local and remote boxes I work on both
have `/opt` folders so moving to that folder would reset my working
directory and poof goes my helpful file completion from `ido`.

I noticed that when I created a shell session from a dired buffer open
to a remote directory, the default directory value actually mapped
perfectly with what was going on in the remote shell. It wasn't
confused by `cd` or identical directories or anything - it followed
bash's working directory perfectly and was quite helpful. So, I tried
to figure out a way to tap into `shell's` default directory mechanism.

    (defun get-user-for-remote-box ()
      (interactive)
      (let ((ssh-config (get-file-as-string ssh-config-path) )
            (ssh-remote-info)
            (ssh-user-remote-pairs))
        (while ssh-config
          (let ((host-line (car ssh-config))
                (user-line (caddr ssh-config)))
            (if (and (string-match-p "Host " host-line)
                     (not (string-match-p "*" host-line))
                     (not (string-match-p "*" user-line))
                     (not (string-match-p "^# " host-line))
                     (not (string-match-p "^# " user-line)))
                (add-to-list 'ssh-user-remote-pairs
                             `(,(car (last (split-string host-line " ")))
                               ,(car (last (split-string user-line " "))))))
            (setq ssh-config (cdr ssh-config))))
        ssh-user-remote-pairs))

    (defun open-ssh-connection (&optional pfx)
      (interactive "p")
      (let ((remote-info (get-user-for-remote-box))
            (buffer "*ssh-")
            (old-default-directory default-directory)
            (box))
        (setq box (ido-completing-read "Which box: " (mapcar 'car remote-info)))
        (setq buffer (concat buffer box "*"))
        (save-window-excursion
          (setq default-directory (concat "/ssh:" box ":/home/" (cadr (assoc box remote-info)) "/"))
          (shell (generate-new-buffer-name buffer))
          (setq default-directory old-default-directory))
        (if (eq pfx 4)
            (pop-to-buffer buffer)
          (switch-to-buffer buffer))
        (set-process-query-on-exit-flag (get-buffer-process buffer) nil)))
