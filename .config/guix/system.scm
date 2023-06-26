(use-modules (gnu)
             (gnu packages linux)
             (gnu packages freedesktop)
             (nongnu packages linux))

(use-package-modules glib certs xdisorg xorg)
(use-service-modules base desktop networking ssh xorg)

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/Bahia")
 (keyboard-layout (keyboard-layout "br"))
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
                  elogind
                  libinput
                  nss-certs
                  xf86-input-evdev
                  xf86-input-keyboard
                  xf86-input-libinput
                  xf86-input-libinput
                  xf86-input-mouse
                  xf86-input-synaptics
                  xf86-video-fbdev
                  xf86-video-intel
                  xf86-video-intel
                  xf86-video-vesa
                  %base-packages))

 (services
  (cons* (service dhcp-client-service-type)
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
                               "24fd9adc-c6cb-480a-95cb-ce0b76c8f933")))))

 (file-systems (cons* (file-system
                       (mount-point "/home")
                       (device (uuid
                                "1e13bd7c-3822-4675-9b05-9fe2d6330207"
                                'ext4))
                       (type "ext4"))
                      (file-system
                       (mount-point "/boot")
                       (device (uuid
                                "c776ac14-6a55-48ac-9026-413e80ab4025"
                                'ext4))
                       (type "ext4"))
                      (file-system
                       (mount-point "/")
                       (device (uuid
                                "f3449f5a-1b4a-4f18-8161-5d97854c29b6"
                                'ext4))
                       (type "ext4")) %base-file-systems)))
