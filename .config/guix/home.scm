(use-modules (gnu home)
             (gnu services)
             (gnu packages)
             (guix gexp)
             (gnu home services)
             (gnu home services xdg)
             (gnu packages chromium)
             (gnu packages curl)
             (gnu packages dunst)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages fonts)
             (gnu packages fontutils)
             (gnu packages gnome)
             (gnu packages gnome-xyz)
             (gnu packages gnupg)
             (gnu packages lisp)
             (gnu packages mpd)
             (gnu packages ssh)
             (gnu packages suckless)
             (gnu packages telegram)
             (gnu packages text-editors)
             (gnu packages version-control)
             (gnu packages video)
             (gnu packages xdisorg)
             (gnu packages xorg)
             (gnu packages wm))

(home-environment
 (packages (list curl
                 dunst
                 emacs
                 emacs-pdf-tools
                 emacs-vterm
                 font-awesome
                 font-google-noto
                 font-google-noto-emoji
                 font-google-noto-sans-cjk
                 font-google-noto-serif-cjk
                 font-iosevka
                 font-iosevka-aile
                 fontconfig
                 git
                 gnupg
                 libnotify
                 maim
                 materia-theme
                 mg
                 mpd
                 mpd-mpc
                 mpv
                 numlockx
                 openssh
                 pinentry-emacs
                 sbcl
                 slock
                 slop
                 stumpwm
                 sx
                 telegram-desktop
                 ungoogled-chromium
                 xclip
                 xhost
                 xset
                 xsetroot
                 youtube-dl))

 (services
  (list (service home-xdg-user-directories-service-type
                 (home-xdg-user-directories-configuration
                  (desktop     "$HOME/desktop")
                  (documents   "$HOME/documents")
                  (download    "$HOME/downloads")
                  (music       "$HOME/musics")
                  (pictures    "$HOME/pictures")
                  (publicshare "$HOME/public")
                  (templates   "$HOME/templates")
                  (videos      "$HOME/videos"))))))
