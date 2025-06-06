(define-module (cs6120 channels)
  #:use-module (guix channels)
  #:export (core-channels))

(define core-channels
  (list (channel
         (name 'rde)
         (url "https://git.sr.ht/~abcdw/rde")
         (branch "master")
         (commit
          "b81a1b5c8b3b7e552be41cb62645ee4514309885")
         (introduction
          (make-channel-introduction
           "257cebd587b66e4d865b3537a9a88cccd7107c95"
           (openpgp-fingerprint
            "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
        (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit
          "e124661486ec722b5c09a94b416f0104b9dde5a4")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
        (channel
         (name 'non-guix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (commit
          "ac1f7b074ea47e8a330e3475732c2e905928df35")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))))

core-channels
