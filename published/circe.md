I recently [heard about a new version]() of [`circe`]() coming out so I went to try it out. I don't remember why I stopped using `erc` but afaik they both seem fine to use.

<pre><code class="lisp">(package-install 'circe)
(setq circe-network-options
      `(("Freenode"
         :nick "dgempesaw"
         :channels ("#emacs" "#selenium")
         :nickserv-password ,freenode-password
         )))
(circe)
</code></pre>

That should be enough to get you up and running and autoconnected to `#emacs` and `#selenium`. You'll have to `setq` your password in freenode-password, of course. Unfortunately, circe comes with `tracking-mode` which fights with `ace-jump-mode` for power over the `C-c C-SPC` keybinding. I added `ace-jump-mode` to that as well as its default at `C-c SPC` to help me whenever I'm clusmy. So, I'll just redefine the keys in the `tracking-mode-map` to my liking:

<pre><code class="lisp">(eval-after-load "tracking"
  '(progn
     (define-key tracking-mode-map (kbd "C-c C-SPC") 'ace-jump-mode)
     (define-key tracking-mode-map (kbd "C-c C-@") 'ace-jump-mode)
     (define-key tracking-mode-map (kbd "C-x C-j C-k") 'tracking-next-buffer)))
</code></pre>

`C-x C-j C-k` is close to jabber's `C-x C-j C-l` - all the new-message-buffer-cycling keys together, please! Otherwise, no complaints really, a pleasant time was had by all.

In fact, my first day idling in `#emacs`, I found out that you can bind the function keys sequentially like prefix keys, which OF COURSE makes sense but it opens up a whole new set of keybindings. For example,

<pre><code class="lisp">(global-set-key (kbd "&lt;f5&gt; &lt;f6&gt;") 'do-something-cool)
(global-set-key (kbd "&lt;f5&gt; &lt;f5&gt;") 'do-someting-else-cool)
</code></pre>

Is that not the coolest? I felt kind of silly for a bit but luckily Fuco hadn't realized it either, and people write about Fuco all the time so nyahhh!
