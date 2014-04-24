A while back, I posted a couple defuns to help with ssh'ing to remote
boxes when you've already configured your `~/.ssh/config` to have the
necessary information about the boxes. It was pretty useful at the
time, but it didn't do anything about fixing the default directory to
help with file autosuggests on the remote box, which are pretty
useful.

Recently, I tried added a single `(cd)` at the end of the defun to
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

As it turns out, as is usually the case, the directory tracking is
reasonably clever on its own (although `popd` and `pushd` with
symlinks may trick it), as long as I set it up correctly in the first
place. Simply setting the `default-directory` to the home directory on the
remote box allows it to track with my CDs in the remote shell, and
`find-file` suggests from the current folder on the remote shell.

    (defun open-ssh-connection (&optional pfx)
      (interactive)
      (with-temp-buffer
        (let* ((remote-info (get-user-for-remote-box))
              (box (if (eq nil pfx)
                       (ido-completing-read
                        "Which box: " (mapcar 'car remote-info))
                     pfx))
              (buffer (concat "*shell<" box ">*"))
              (default-directory
                (concat "/" box ":/home/"
                        (cadr (assoc box remote-info)) "/")))
          (shell buffer)
          (set-process-query-on-exit-flag
           (get-buffer-process buffer) nil))))

Wrapping it in a `with-temp-buffer` takes care of the case where
you're trying to use this function while in a remote buffer. In those
cases, it will try to ssh from the remote box to the new remote box,
and of course it won't be able to find the required ssh entry in the
remote's `~/ssh/config`. Using a temp buffer resets the `cwd` to the
local box prior to setting the `default directory`.
