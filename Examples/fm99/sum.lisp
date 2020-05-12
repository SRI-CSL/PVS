(in-package 'PVS)

(DEFUN |sum_nats_0| ()
  #'(LAMBDA (|n|)
      (IF (THE BOOLEAN (PVS__= (THE (INTEGER 0) |n|) 0))
          0
        (PVS__+ (THE (INTEGER 0) |n|)
                (THE (INTEGER 0)
                     (|_sum_nats_0| (PVS__-
                                     (THE (INTEGER 0) |n|)
                                     1)))))))
(DEFUN |_sum_nats_0| (|n|)
  (DECLARE (TYPE (INTEGER 0) |n|))
  (IF (THE BOOLEAN (PVS__= (THE (INTEGER 0) |n|) 0))
      0
    (PVS__+ (THE (INTEGER 0) |n|)
            (THE (INTEGER 0)
                 (|_sum_nats_0| (PVS__- (THE (INTEGER 0) |n|) 1))))))
(DEFUN |sum_nats!_0| (|n|)
  (DECLARE (TYPE (INTEGER 0) |n|))
  (IF (THE BOOLEAN (PVS__= (THE (INTEGER 0) |n|) 0))
      0
    (PVS__+ (THE (INTEGER 0) |n|)
            (THE (INTEGER 0)
                 (|sum_nats!_0| (PVS__- (THE (INTEGER 0) |n|) 1))))))