(use-modules (gnu home)
             (gnu services)
             (gnu packages)
             (gnu home services)
             (gnu home services xdg)
             (guix gexp))

(home-environment
 (packages (specifications->packages
            (list "curl"
                  "dunst"
                  "emacs"
                  "emacs-flymake-shellcheck"
                  "emacs-pdf-tools"
                  "emacs-vterm"
                  "font-awesome"
                  "font-google-noto"
                  "font-google-noto-emoji"
                  "font-google-noto-sans-cjk"
                  "font-google-noto-serif-cjk"
                  "font-iosevka"
                  "font-iosevka-aile"
                  "fontconfig"
                  "git"
                  "gnupg"
                  "isync"
                  "libnotify"
                  "maim"
                  "materia-theme"
                  "mg"
                  "mpd"
                  "mpd-mpc"
                  "mpv"
                  "mu"
                  "numlockx"
                  "openssh"
                  "pinentry"
                  "sbcl"
                  "slock"
                  "slop"
                  "stumpwm"
                  ;"sx"
                  "telegram-desktop"
                  "ungoogled-chromium"
                  "unzip"
                  "xclip"
                  "xhost"
                  "xinit"
                  "xset"
                  "xsetroot"
                  "youtube-dl"
                  "zip")))

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