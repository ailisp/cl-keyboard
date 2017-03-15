# cl-keyboard
cl-keyboard is a useful keyboard remap for Common Lispers on Windows. After load it, the `[` and `]` keys will be remap to `(` and `)`, `(` and `)`, which are `Shift+9` and `Shift+0` will be remap to `[` and `]`. The left control (`Ctrl`) is swapped with `Capslock`. The keyboard change will be affect to any program on Windows, unless you close the Lisp process. If you want to automatically load this small utility, you can append this to your Lisp init file (e.g. `~/.sbclrc` for SBCL and `~/ccl-init.lisp` for CCL):
```lisp
#+quicklisp
(let ((cl-keyboard-init (merge-pathnames "common-lisp/cl-keyboard.lisp"
                        (user-homedir-pathname))))
  (when (probe-file cl-keyboard-init)
    (load cl-keyboard-init)))
```
## Why this remap?
This is actually the keyboard map similar to [Symbolics Lisp Machine's keyboard](https://en.wikipedia.org/wiki/Space-cadet_keyboard). In this keyboard map, you need only move your little finger of right hand a little from `P` key to input a `(` or `)`, without press `Shift`. As Common Lisp use `[]` little, it's more convinient to use them to type `()`, but this may not for Scheme and Clojure programmers. As for swap `Capslock` and `LCtrl`, it's a common practice for most emacs users. 

## What about on Linux?
I put my `.Xmodmap` file in this responsitory. If you like it, install xmodmap via your system's package manager and put `.Xmodmap` to your home folder, it will automatically work for lightdm, kdm and gdm, other desktop managers are not tested. If it's not automatically start, just run or put to your system startup file `xmodmap ~/.Xmodmap`. It will work just as cl-keyboard on Windows.

## A note on cl-keyboard
When I start maintain [Graphic-Forms](https://gitlab.common-lisp.net/byao/Graphic-Forms), I had to work on windows. On Linux, I can use a single `.Xmodmap` file to swap these keys, but on Windows, there seems no software can finish this remap. There are several candidates, one of them is a group of remap tools, such as sharpkeys, but they can only swap key X to Y, not X to Shift-Y. Another kind of tool is listen to what user input and send modified keyboard events, such as AutoHotKey. This works similar to cl-keyboard, but when I tried to define such remap rules in AutoHotKey script, either would I ran into an infinite loop or `[` remap to `(` but also `Shift+[` remap to `(`: the `{` key got lost! A third way is to do some remap in `.emacs` file, but it doesn't work for keystrokes, such as `C-(` or `M-(`, and when you are out of emacs it changed back -- you have to remember two sets of keys. So I have to write this small tool to precisely cope with low level windows keyboard events.

(I hope this small utility help you writing Common Lisp \:)
