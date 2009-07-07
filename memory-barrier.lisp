(cl:in-package :cilk)

(sb-c::defknown mfence () (values))
;; X86-64 memory fence instruction
(sb-vm::define-vop (mfence)
  (:policy :fast-safe)
  (:translate mfence)
  (:effects)
  (:affected)
  (:generator 0
              (sb-vm::inst sb-vm::byte #x0f)
              (sb-vm::inst sb-vm::byte #xae)
              (sb-vm::inst sb-vm::byte #xf0)))

