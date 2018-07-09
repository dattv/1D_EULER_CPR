PROGRAM MAIN 
    ! ================================================== INCLUDE MODULE ==================================================
    USE MODULE_NONDIMENSIONALNUMBER
    USE MODULE_CORRECTIONPOLYNOMIAL
    USE MODULE_COMPUTATIONALSYSTEM
    USE MODULE_MATERIALCOMPONENT
    USE MODULE_SOLVERINFORMATION
    USE MODULE_MODEL
    
    USE MODULE_INITIALCONDITION
    USE MODULE_SOLVER
    ! ===================================================== DECLARE ======================================================
    IMPLICIT NONE
    
    INTEGER(IP) :: ORDER, RK_ORDER
    REAL(WP)    :: TIME_START, TIME_END
    REAL(WP)    :: CFL, DT
    INTEGER(IP) :: NELE, NVAL
    REAL(WP)    :: LB, RB
    CHARACTER(LEN = 20) :: FILE_OUT, CITER
    INTEGER(IP) :: ITER
    
    TYPE(NONDIMENSIONALNUMBER), TARGET  :: NON_INFO(1)
    TYPE(CORRECTIONPOLYNOMIAL), TARGET  :: CP_COEF(1)
    TYPE(COMPUTATIONALSYSTEM), TARGET   :: COMSYS(1)
    TYPE(MATERIALCOMPONENT), TARGET     :: MATERIALINFO(1)
    TYPE(SOLVERINFORMATION), TARGET     :: SOLVERINFO(1)
    TYPE(MODEL) :: MODEL_
    REAL(WP)    :: T1, T2, TIME
    
    ! ======================================================= BODY =======================================================
    ORDER       = 6         ;    RK_ORDER    = 5        ;
    TIME_START  = 0._WP     ;    TIME_END    = 0.2_WP   ;
    CFL         = 0.05_WP   ;    DT          = 5D-6     ;
    NELE        = 100       ;    NVAL        = 3        ;
    LB          = 0._WP     ;    RB          = 1._WP    ;
    
    CALL CP_COEF(1)%MAKE(ORDER)
    CALL COMSYS(1)%MAKE(ORDER, CP_COEF(1))
    CALL SOLVERINFO(1)%MAKE(0, RK_ORDER, TIME_START, TIME_END, CFL, DT)
    
    CALL MODEL_%MAKE(NELE, NVAL, LB, RB, COMSYS, MATERIALINFO, CP_COEF, NON_INFO, SOLVERINFO)
    
    ! ==================================================== COMPUTE PI ====================================================
    MODEL_%ELE(1)%MATERIALINFO%PI = 4._WP*ATAN(1._WP)
    
    CALL INITIAL_EULER_1D(1, MODEL_%NELE, MODEL_%ELE)
    
    ! ===========================================SET UP BOUNDARY CONDITION ===============================================
    MODEL_%BOUND_ELE(1) = MODEL_%ELE(1)
    MODEL_%BOUND_ELE(2) = MODEL_%ELE(MODEL_%NELE)
    
    MODEL_%ELE(1)%LEFT              => MODEL_%BOUND_ELE(1:1)
    MODEL_%ELE(MODEL_%NELE)%RIGHT   => MODEL_%BOUND_ELE(2:2)
    !MODEL_%ELE(1)%LEFT => MODEL_%BOUND_ELE(1:2)
    
    ! ======================================== PRINT OUT INITIAL CONDITION ===============================================
    CALL MODEL_%OUTPUT("INITIAL.TEC")
    
    ! ==================================================== SIMULATION ====================================================
    CALL CPU_TIME(T1)
    TIME = 0._WP;   ITER = 1
    DO WHILE(TIME <= TIME_END)
        TIME = TIME + SOLVERINFO(1)%DT
        
        CALL SOLVER(SOLVERINFO, 1, MODEL_%NELE, MODEL_%NELE, MODEL_%ELE)
        
        IF (MOD(ITER, 1) == 0) THEN 
            WRITE(FILE_OUT, "(I4)") ITER
            
            WRITE(*, *) "PRINT OUT FILE: ", "OUTPUT"//TRIM(FILE_OUT)//".TEC", "      TIME :", TIME
            CALL MODEL_%OUTPUT("OUTPUT"//TRIM(FILE_OUT)//".TEC")
            CONTINUE
        END IF
        
        ITER = ITER + 1
        CONTINUE
    END DO
    CALL CPU_TIME(T2)
    WRITE(*, *)" TIME ELAPSED: ", T2 - T1
    
    ! ================================================== END SIMULATION ==================================================
    
    STOP
END PROGRAM     