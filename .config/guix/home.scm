(use-modules
 (gnu packages)
 (gnu services)
 (gnu home services gnupg)
 (gnu home services xdg)
 (gnu packages admin)
 (gnu packages bittorrent)
 (gnu packages chromium)
 (gnu packages compression)
 (gnu packages curl)
 (gnu packages dunst)
 (gnu packages emacs)
 (gnu packages emacs-xyz)
 (gnu packages fonts)
 (gnu packages fontutils)
 (gnu packages gnome)
 (gnu packages gnome-xyz)
 (gnu packages gnupg)
 (gnu packages imagemagick)
 (gnu packages java)
 (gnu packages lisp)
 (gnu packages mail)
 (gnu packages mpd)
 (gnu packages package-management)
 (gnu packages ssh)
 (gnu packages telegram)
 (gnu packages video)
 (gnu packages wm)
 (gnu packages xdisorg)
 (gnu packages xorg)
 (nongnu packages clojure))

(home-environment
 (packages (cons* curl
                  dunst
                  emacs
                  emacs-flymake-shellcheck
                  emacs-pdf-tools
                  emacs-vterm
                  ffmpeg
                  font-awesome
                  font-google-noto
                  font-google-noto-emoji
                  font-google-noto-sans-cjk
                  font-google-noto-serif-cjk
                  font-iosevka
                  font-iosevka-aile
                  fontconfig
                  isync
                  imagemagick
                  leiningen
                  libnotify
                  maim
                  materia-theme
                  mpd
                  mpd-mpc
                  mpdscribble
                  mpv
                  mu
                  numlockx
                  `(,openjdk "jdk")
                  openssh
                  pinentry-emacs
                  sbcl
                  slop
                  stow
                  stumpwm
                  sx
                  telegram-desktop
                  transmission
                  tree
                  ungoogled-chromium
                  unzip
                  xclip
                  xhost
                  xset
                  xsetroot
                  youtube-dl
                  (specifications->packages
                   (list "zip"))))

 (services
  (list (service home-gpg-agent-service-type
                 (home-gpg-agent-configuration
                  (extra-content (format #f "~@{~a~%~}"
                                         "allow-emacs-pinentry"
                                         "allow-loopback-pinentry"))))
        
        (service home-xdg-user-directories-service-type
                 (home-xdg-user-directories-configuration
                  (desktop     "$HOME/desktop")
                  (documents   "$HOME/documents")
                  (download    "$HOME/downloads")
                  (music       "$HOME/musics")
                  (pictures    "$HOME/pictures")
                  (publicshare "$HOME/public")
                  (templates   "$HOME/templates")
                  (videos      "$HOME/videos"))))))
