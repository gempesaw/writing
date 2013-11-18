In a previous post, I talked about [adding syntax-highlighting to this blog](). It's working out quite well for me, but I wanted to pull it into my composition workflow better. My current suboptimal workflow is to compose markdown in Emacs and copy/paste into Tumblr's web app. (Tumblesocks is having connection problems that I don't feel like figuring out quite yet.) When I `markdown-preview`, I want to be able to see my code in all its syntax highlighted glory. Let's look at `markdown-preview`:

    (defun markdown-preview (&optional output-buffer-name)
      "Run \`markdown' on the current buffer and preview the output in a browser."
      (interactive)
      (browse-url-of-buffer (markdown markdown-output-buffer-name)))


So, looking into that fourth line, we call `markdown` with the argument `markdown-output-buffer-name`. `markdown` itself runs markdown on the current buffer and outputs to the optional argument: in this case `markdown-output-buffer-name`. Then, we pass that temporary buffer to `browse-url-of-buffer` which opens it up in Chrome and voila! we see a preview of the markdown under composition.

What I wanted to do was to add the same style and script sources that I added to the tumblr theme. That way, my markdown preview would have the same syntax highlighting that my tumblr would have! So, I wrote my own `markdown-preview` function:

    (defun markdown-preview-with-syntax-highlighting (&optional output-buffer-name)
      "Run `markdown' on the current buffer and preview the output in a browser."
      (interactive)
      (browse-url-of-buffer
       (with-current-buffer (markdown markdown-output-buffer-name)
         (goto-char (point-min))
         (if (> (length markdown-css-path) 0)
             (insert "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
                     markdown-css-path
                     "\"  />\n"))

         (if (> (length markdown-script-path) 0)
             (progn
               (insert "<script type=\"text/javascript\" src=\""
                     markdown-script-path
                     "\"  /></script>\n")
               (insert "<script type=\"text/javascript\">hljs.initHighlightingOnLoad();</script>")))
         markdown-output-buffer-name)))

Where I want to jump in is after markdown has processed my file but before we send the output buffer to `browse-url-of-buffer`. `(markdown markdown-output-buffer-name)` returns the output buffer, so I catch that into the first argument of `with-current-buffer`. I go to the beginning of the buffer and determine whether or not I know where the desired css and scripts are. Assuming they're defined somewhere else...

    (setq markdown-css-path "/opt/highlight.js/src/styles/ir_black.css")
    (setq markdown-script-path "/opt/highlight.js/build/highlight.pack.js")

the function will insert analogous style and script tags that highlight the text!

Last but not least, instead of calling `markdown-preview` as my compile command for markdown buffers, I should call my improved version:

    (add-to-list 'smart-compile-alist '("\\.md\\'" . (markdown-preview-with-syntax-highlighting)))

Local automatic syntax highlighting in markdown previews complete!
