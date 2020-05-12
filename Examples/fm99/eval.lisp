(in-package 'PVS)

(DEFUN |valof_0| ()
  #'(LAMBDA (|v|)
      #'(LAMBDA (|mem|)
          (THE INTEGER
               (IF (THE BOOLEAN (|const_0-P| |v|))
                   (THE INTEGER (|const_0-n| |v|))
                 (IF (THE BOOLEAN (|varbl_0-P| |v|))
                     (THE INTEGER
                          (PVS-FUNCALL |mem|
                                       (THE
                                        (INTEGER 0 *)
                                        (|varbl_0-a| |v|))))
                   (IF (THE BOOLEAN (+_0-P |v|))
                       (PVS__+ (THE
                                INTEGER
                                (PVS-FUNCALL
                                 (|_valof_0| (|+_0-x| |v|))
                                 |mem|))
                               (THE
                                INTEGER
                                (PVS-FUNCALL
                                 (|_valof_0| (|+_0-y| |v|))
                                 |mem|)))
                     (IF (THE BOOLEAN (-_0-P |v|))
                         (PVS__- (THE
                                  INTEGER
                                  (PVS-FUNCALL
                                   (|_valof_0| (|-_0-x| |v|))
                                   |mem|))
                                 (THE
                                  INTEGER
                                  (PVS-FUNCALL
                                   (|_valof_0| (|-_0-y| |v|))
                                   |mem|)))
                       (PVS_- (THE INTEGER
                                   (PVS-FUNCALL
                                    (|_valof_0| (|~_0-x| |v|))
                                    |mem|)))))))))))
(DEFUN |_valof_0| (|v|)
  #'(LAMBDA (|mem|)
      (THE INTEGER
           (IF (THE BOOLEAN (|const_0-P| |v|))
               (THE INTEGER (|const_0-n| |v|))
             (IF (THE BOOLEAN (|varbl_0-P| |v|))
                 (THE INTEGER
                      (PVS-FUNCALL |mem|
                                   (THE
                                    (INTEGER 0 *)
                                    (|varbl_0-a| |v|))))
               (IF (THE BOOLEAN (+_0-P |v|))
                   (PVS__+ (THE INTEGER
                                (PVS-FUNCALL
                                 (|_valof_0| (|+_0-x| |v|))
                                 |mem|))
                           (THE INTEGER
                                (PVS-FUNCALL
                                 (|_valof_0| (|+_0-y| |v|))
                                 |mem|)))
                 (IF (THE BOOLEAN (-_0-P |v|))
                     (PVS__- (THE INTEGER
                                  (PVS-FUNCALL
                                   (|_valof_0| (|-_0-x| |v|))
                                   |mem|))
                             (THE INTEGER
                                  (PVS-FUNCALL
                                   (|_valof_0| (|-_0-y| |v|))
                                   |mem|)))
                   (PVS_- (THE INTEGER
                               (PVS-FUNCALL
                                (|_valof_0| (|~_0-x| |v|))
                                |mem|))))))))))
(DEFUN |valof!_0| (|v|)
  #'(LAMBDA (|mem|)
      (THE INTEGER
           (IF (THE BOOLEAN (|const_0-P| |v|))
               (THE INTEGER (|const_0-n| |v|))
             (IF (THE BOOLEAN (|varbl_0-P| |v|))
                 (THE INTEGER
                      (PVS-FUNCALL |mem|
                                   (THE
                                    (INTEGER 0 *)
                                    (|varbl_0-a| |v|))))
               (IF (THE BOOLEAN (+_0-P |v|))
                   (PVS__+ (THE INTEGER
                                (PVS-FUNCALL
                                 (|valof!_0| (|+_0-x| |v|))
                                 |mem|))
                           (THE INTEGER
                                (PVS-FUNCALL
                                 (|valof!_0| (|+_0-y| |v|))
                                 |mem|)))
                 (IF (THE BOOLEAN (-_0-P |v|))
                     (PVS__- (THE INTEGER
                                  (PVS-FUNCALL
                                   (|valof!_0| (|-_0-x| |v|))
                                   |mem|))
                             (THE INTEGER
                                  (PVS-FUNCALL
                                   (|valof!_0| (|-_0-y| |v|))
                                   |mem|)))
                   (PVS_- (THE INTEGER
                               (PVS-FUNCALL
                                (|valof!_0| (|~_0-x| |v|))
                                |mem|))))))))))
(DEFUN |runtime_0| ()
  #'(LAMBDA (|s|)
      (THE (INTEGER 0)
           (IF (THE BOOLEAN (|assign_0-P| |s|))
               1
             (IF (THE BOOLEAN (|seq_0-P| |s|))
                 (PVS__+ (THE (INTEGER 0)
                              (|_runtime_0| (|seq_0-p| |s|)))
                         (THE (INTEGER 0)
                              (|_runtime_0| (|seq_0-q| |s|))))
               (IF (THE BOOLEAN (|ifelse_0-P| |s|))
                   (PVS__+ (|_max_0| (THE
                                      (INTEGER 0)
                                      (|_runtime_0|
                                       (|ifelse_0-i| |s|)))
                                     (THE
                                      (INTEGER 0)
                                      (|_runtime_0|
                                       (|ifelse_0-e| |s|))))
                           1)
                 (PVS__+ (PVS__* (THE (INTEGER 0) (|for_0-l| |s|))
                                 (THE
                                  (INTEGER 0)
                                  (|_runtime_0| (|for_0-b| |s|))))
                         1)))))))
(DEFUN |_runtime_0| (|s|)
  (THE (INTEGER 0)
       (IF (THE BOOLEAN (|assign_0-P| |s|))
           1
         (IF (THE BOOLEAN (|seq_0-P| |s|))
             (PVS__+ (THE (INTEGER 0) (|_runtime_0| (|seq_0-p| |s|)))
                     (THE (INTEGER 0) (|_runtime_0| (|seq_0-q| |s|))))
           (IF (THE BOOLEAN (|ifelse_0-P| |s|))
               (PVS__+ (|_max_0| (THE
                                  (INTEGER 0)
                                  (|_runtime_0| (|ifelse_0-i| |s|)))
                                 (THE
                                  (INTEGER 0)
                                  (|_runtime_0| (|ifelse_0-e| |s|))))
                       1)
             (PVS__+ (PVS__* (THE (INTEGER 0) (|for_0-l| |s|))
                             (THE (INTEGER 0)
                                  (|_runtime_0| (|for_0-b| |s|))))
                     1))))))
(DEFUN |runtime!_0| (|s|)
  (THE (INTEGER 0)
       (IF (THE BOOLEAN (|assign_0-P| |s|))
           1
         (IF (THE BOOLEAN (|seq_0-P| |s|))
             (PVS__+ (THE (INTEGER 0) (|runtime!_0| (|seq_0-p| |s|)))
                     (THE (INTEGER 0) (|runtime!_0| (|seq_0-q| |s|))))
           (IF (THE BOOLEAN (|ifelse_0-P| |s|))
               (PVS__+ (|max!_0| (THE
                                  (INTEGER 0)
                                  (|runtime!_0| (|ifelse_0-i| |s|)))
                                 (THE
                                  (INTEGER 0)
                                  (|runtime!_0| (|ifelse_0-e| |s|))))
                       1)
             (PVS__+ (PVS__* (THE (INTEGER 0) (|for_0-l| |s|))
                             (THE (INTEGER 0)
                                  (|runtime!_0| (|for_0-b| |s|))))
                     1))))))
(DEFUN |exec_0| ()
  #'(LAMBDA (|s|)
      #'(LAMBDA (|mem|)
          (IF (THE BOOLEAN (|assign_0-P| |s|))
              (LET ((A71 (THE (INTEGER 0 *) (|assign_0-a| |s|))))
                (LET ((N70
                       (THE INTEGER
                            (PVS-FUNCALL (|_valof_0|
                                          (|assign_0-e| |s|))
                                         |mem|)))
                      (E69 |mem|))
                  (PVS-OUTER-ARRAY-UPDATE E69 A71 N70
                    (THE (INTEGER 0)
                         (PVS__+ (THE (INTEGER 0) (|_n_0|)) 1)))))
            (IF (THE BOOLEAN (|seq_0-P| |s|))
                (PVS-FUNCALL (|_exec_0| (|seq_0-q| |s|))
                             (PVS-FUNCALL (|_exec_0| (|seq_0-p| |s|))
                                          |mem|))
              (IF (THE BOOLEAN (|ifelse_0-P| |s|))
                  (IF (THE BOOLEAN
                           (PVS__/= (THE
                                     INTEGER
                                     (PVS-FUNCALL
                                      (|_valof_0| (|ifelse_0-t| |s|))
                                      |mem|))
                                    0))
                      (PVS-FUNCALL (|_exec_0| (|ifelse_0-i| |s|))
                                   |mem|)
                    (PVS-FUNCALL (|_exec_0| (|ifelse_0-i| |s|)) |mem|))
                (IF (THE BOOLEAN
                         (PVS__= (THE (INTEGER 0) (|for_0-l| |s|)) 0))
                    |mem|
                  (LET ((|m|
                         (PVS-FUNCALL (|_exec_0| (|for_0-b| |s|))
                                      |mem|)))
                    (DECLARE (TYPE NIL |m|))
                    (PVS-FUNCALL (|_exec_0|
                                  (|MAKE-for_0|
                                   (PVS__-
                                    (THE (INTEGER 0) (|for_0-l| |s|))
                                    1)
                                   (|for_0-b| |s|)))
                                 |m|)))))))))
(DEFUN |_exec_0| (|s|)
  #'(LAMBDA (|mem|)
      (IF (THE BOOLEAN (|assign_0-P| |s|))
          (LET ((A47 (THE (INTEGER 0 *) (|assign_0-a| |s|))))
            (LET ((N45
                   (THE INTEGER
                        (PVS-FUNCALL (|_valof_0| (|assign_0-e| |s|))
                                     |mem|)))
                  (E44 |mem|))
              (PVS-OUTER-ARRAY-UPDATE E44 A47 N45
                (THE (INTEGER 0)
                     (PVS__+ (THE (INTEGER 0) (|_n_0|)) 1)))))
        (IF (THE BOOLEAN (|seq_0-P| |s|))
            (PVS-FUNCALL (|_exec_0| (|seq_0-q| |s|))
                         (PVS-FUNCALL (|_exec_0| (|seq_0-p| |s|))
                                      |mem|))
          (IF (THE BOOLEAN (|ifelse_0-P| |s|))
              (IF (THE BOOLEAN
                       (PVS__/= (THE
                                 INTEGER
                                 (PVS-FUNCALL
                                  (|_valof_0| (|ifelse_0-t| |s|))
                                  |mem|))
                                0))
                  (PVS-FUNCALL (|_exec_0| (|ifelse_0-i| |s|)) |mem|)
                (PVS-FUNCALL (|_exec_0| (|ifelse_0-i| |s|)) |mem|))
            (IF (THE BOOLEAN
                     (PVS__= (THE (INTEGER 0) (|for_0-l| |s|)) 0))
                |mem|
              (LET ((|m|
                     (PVS-FUNCALL (|_exec_0| (|for_0-b| |s|)) |mem|)))
                (DECLARE (TYPE NIL |m|))
                (PVS-FUNCALL (|_exec_0| (|MAKE-for_0|
                                         (PVS__-
                                          (THE
                                           (INTEGER 0)
                                           (|for_0-l| |s|))
                                          1)
                                         (|for_0-b| |s|)))
                             |m|))))))))
(DEFUN |exec!_0| (|s|)
  #'(LAMBDA (|mem|)
      (IF (THE BOOLEAN (|assign_0-P| |s|))
          (LET ((LHS65 (THE (INTEGER 0 *) (|assign_0-a| |s|))))
            (LET ((RHS63
                   (THE INTEGER
                        (PVS-FUNCALL (|valof!_0| (|assign_0-e| |s|))
                                     |mem|)))
                  (E64
                   (MK-FUN-ARRAY |mem|
                                 (THE
                                  (INTEGER 0)
                                  (PVS__+
                                   (THE (INTEGER 0) (|n!_0|))
                                   1)))))
              (LET ((E66
                     (MK-FUN-ARRAY E64
                                   (THE
                                    (INTEGER 0)
                                    (PVS__+
                                     (THE (INTEGER 0) (|n!_0|))
                                     1)))))
                (SETF (SVREF E66 LHS65) RHS63)
                E66)
              E64))
        (IF (THE BOOLEAN (|seq_0-P| |s|))
            (PVS-FUNCALL (|exec!_0| (|seq_0-q| |s|))
                         (PVS-FUNCALL (|exec!_0| (|seq_0-p| |s|))
                                      |mem|))
          (IF (THE BOOLEAN (|ifelse_0-P| |s|))
              (IF (THE BOOLEAN
                       (PVS__/= (THE
                                 INTEGER
                                 (PVS-FUNCALL
                                  (|valof!_0| (|ifelse_0-t| |s|))
                                  |mem|))
                                0))
                  (PVS-FUNCALL (|exec!_0| (|ifelse_0-i| |s|)) |mem|)
                (PVS-FUNCALL (|exec!_0| (|ifelse_0-i| |s|)) |mem|))
            (IF (THE BOOLEAN
                     (PVS__= (THE (INTEGER 0) (|for_0-l| |s|)) 0))
                |mem|
              (LET ((|m|
                     (PVS-FUNCALL (|exec!_0| (|for_0-b| |s|)) |mem|)))
                (DECLARE (TYPE NIL |m|))
                (PVS-FUNCALL (|exec!_0| (|MAKE-for_0|
                                         (PVS__-
                                          (THE
                                           (INTEGER 0)
                                           (|for_0-l| |s|))
                                          1)
                                         (|for_0-b| |s|)))
                             |m|))))))))
(DEFUN |init_0| () (|id_0|))
(DEFUN |_init_0| () (|id_0|))
(DEFUN |init!_0| () (|id_0|))
(DEFUN |zero_0| () (|_K_conversion_0| 0))
(DEFUN |_zero_0| () (|_K_conversion_0| 0))
(DEFUN |zero!_0| () (|K_conversion!_0| 0))