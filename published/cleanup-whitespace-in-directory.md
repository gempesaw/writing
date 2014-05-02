# Cleaning up whitespace in directories with a small elisp

I had a project with an outdated indentation convention. Present-me
recently decided to change bracket conventions from the way past-me
had been doing things. So, I updated my emacs settings so that
[magnar's][] `(cleanup-buffer)` function would properly format the
files. But, there were multiple files in the project and I didn't feel
like manually opening each one just to cleanup (`C-c n`) and save
(`M-s`) it. So, I had emacs do it for me, relying heavily on the
excellent [`f.el`][f] library and the aforementioned
`(cleanup-buffer)`.

    (require 'f)
    (defun cleanup-in-directory (directory &optional match recursive)
      (let* ((matcher (if (not (eq nil match))
                          (lambda (file) (string-match match it))
                        nil))
             (files (f-files directory matcher recursive)))
        (mapc (lambda (file)
                (ignore-errors
                  (find-file file)
                  (cleanup-buffer)
                  (save-buffer)))
              files)))

    (cleanup-in-directory "/opt/honeydew/lib" ".pm$" t)
    (cleanup-in-directory "/opt/honeydew/bin")

It takes a directory, a regex to optionally match against, and a flag
to determine whether or not to recurse the parent directory. Small
problem, small elisp: problem solved :).

I did glance at the changes before committing them, and caught one case
where the formatter got confused and stopped indenting things
entirely, but there was only one such case that I noticed...

[magnar's]: http://whattheemacsd.com/buffer-defuns.el-01.html
[f]: https://github.com/rejeep/f.el
