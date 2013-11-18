I've decided to get a bit OCD about my window sizes in Emacs, and I got tired of hitting `C-x +` to do `balance-windows` all the time. I'm not sure if there's a better way to do this, but I just added some advice around all of the commands I usually use to change the window layout: `split-window-below`, `split-window-right`, and `delete-window`.

    (defadvice split-window-below (after restore-balanace-below activate)
      (balance-windows))

    (defadvice split-window-right (after restore-balance-right activate)
      (balance-windows))

    (defadvice delete-window (after restore-balance activate)
      (balance-windows))

So now my windows automatically balance themselves whenever I `C-x 2` or `C-x 3`, all the time! Nifty. On the off chance that I'd like to manually control the size of a window, I've found `enlarge-window` under `C-x ^`; along with a `C-u` and `C-x z`, I can enlarge a window by a bunch of lines pretty quickly.
