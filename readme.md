About
-----
This is my Emacs configuarion. It has been cobbled and borrowed from people who know more about Emacs and Emacs Lisp than I do. Here are some of my sources:
	http://www.emacswiki.org/emacs/
	http://www.emacswiki.org/emacs/ESSWindowsAdvice
	https://github.com/boorad/emacs
	https://github.com/overtone/live-coding-emacs.git

Audience
--------
You are more likely to find my configurations useful if:
- you come from a Windows + Visual Studio background
- you want to use Emacs primarily for Erlang development
- you want to use Emacs on both Windows and Linux

If you think I've butchered things please let me know.

How to use these files
-----------------------
If you don't already have Emacs install it. I'm using Emacs 23.2.
Clone (or download and extract) this repository so that the directory "emacs-config" is in your home directory (Linux "~/" Windows "%HOME%").
Once you have a "~/emacs-config" directory edit your "~/.emacs" file to require emacs-config by adding these two lines:

<pre>
(add-to-list 'load-path "~/emacs-config")
(require 'my-config)
</pre>