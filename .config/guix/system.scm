(use-modules
 (gnu)
 (gnu packages certs)
 (gnu packages disk)
 (gnu packages file-systems)
 (gnu packages freedesktop)
 (gnu packages glib)
 (gnu packages gnupg)
 (gnu packages linux)
 (gnu packages texinfo)
 (gnu packages version-control)
 (gnu packages xorg)
 (gnu services base)
 (gnu services desktop)
 (gnu services networking)
 (gnu services ssh)
 (gnu services xorg)
 (nongnu packages linux))

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Bahia")
 (keyboard-layout (keyboard-layout "br" "abnt2"))
 (host-name "batatinha")

 (users (cons* (user-account
                (name "crzjp")
                (comment "João Paulo da Cruz")
                (group "users")
                (home-directory "/home/crzjp")
                (supplementary-groups '("wheel" "netdev" "audio" "video" "input" "tty")))
               %base-user-accounts))

 (packages (cons* alsa-lib
                  alsa-utils
                  dbus
                  dosfstools
                  elogind
                  exfat-utils
                  exfatprogs
                  fuse-exfat
                  git
                  gnupg
                  libinput
                  nss-certs
                  texinfo
                  xf86-input-keyboard
                  xf86-input-libinput
                  xf86-input-mouse
                  xf86-input-synaptics
                  xf86-video-intel
                  %base-packages))

 (services
  (cons* (service dhcp-client-service-type)
         (service elogind-service-type)
         (service ntp-service-type)
         (service xorg-server-service-type
                  (xorg-configuration
                   (keyboard-layout keyboard-layout)
                   (extra-config (list "Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
"))))
         (modify-services %base-services
                          (guix-service-type config =>
                                             (guix-configuration
                                              (inherit config)
                                              (substitute-urls
                                               (append (list "https://substitutes.nonguix.org")
                                                       %default-substitute-urls))
                                              (authorized-keys
                                               (append (list (local-file "./signing-key.pub"))
                                                       %default-authorized-guix-keys)))))))

 (bootloader (bootloader-configuration
              (bootloader grub-bootloader)
              (targets (list "/dev/sda"))
              (keyboard-layout keyboard-layout)))

 (swap-devices (list (swap-space
                      (target (uuid
                               "7e6487e3-3edf-4970-89e0-dbc87de8e6a8")))))

 (file-systems (cons* (file-system
                       (mount-point "/")
                       (device (uuid
                                "68c52ef2-2cee-4a73-a234-b44fdda03d8e"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
