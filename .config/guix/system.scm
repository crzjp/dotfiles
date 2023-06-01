;; This is an operating system configuration generated
;; by the graphical installer.
;;
;; Once installation is complete, you can learn and modify
;; this file to tweak the system configuration, and pass it
;; to the 'guix system reconfigure' command to effect your
;; changes.

;; Indicate which modules to import to access the variables
;; used in this configuration.
(use-modules (gnu) (nongnu packages linux))
(use-package-modules glib certs xdisorg xorg)
(use-service-modules base desktop networking ssh xorg)

(define %xorg-libinput-config
  "Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %alsa-asound-config
  "pcm_slave.slavej {
  pcm \"hw:0\"
  channels 2
  rate 44100
}
pcm.plugj {
  type plug
  slave slavej
}
")

(operating-system
 (kernel linux)
 (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/Bahia")
  (keyboard-layout (keyboard-layout "br"))
  (host-name "batatinha")

  ;; The list of user accounts ('root' is implicit).
  (users (cons* (user-account
                  (name "crzjp")
                  (comment "Joao Paulo da Cruz")
                  (group "users")
                  (home-directory "/home/crzjp")
                  (supplementary-groups '("wheel" "netdev" "audio" "video" "input" "tty")))
                %base-user-accounts))

  ;; Packages installed system-wide.  Users can also install packages
  ;; under their own account: use 'guix search KEYWORD' to search
  ;; for packages and 'guix install PACKAGE' to install a package.
  ;(packages (append (list (specification->package "nss-certs"))
  ;                  %base-packages))
  (packages (cons* dbus
                   nss-certs
                   sx
                   xf86-input-libinput
                   xf86-video-intel
                   xhost
                   %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (cons* (service dhcp-client-service-type)
          (service ntp-service-type)
          (service xorg-server-service-type
                   (xorg-configuration
                    (keyboard-layout keyboard-layout)
                    (extra-config (list %xorg-libinput-config))))
          ;; (service alsa-service-type
          ;;          (alsa-configuration
          ;;           (pulseaudio? #f)
          ;;           (extra-options %alsa-asound-config)))
          ;; This is the default list of services we
          ;; are appending to.
          ;%desktop-services
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

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
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
