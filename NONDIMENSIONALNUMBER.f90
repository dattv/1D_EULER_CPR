MODULE  MODULE_NONDIMENSIONALNUMBER
    
    USE MODULE_PRECISION
    
    IMPLICIT NONE
    
    TYPE, PUBLIC :: NONDIMENSIONALNUMBER
    
        REAL(WP)    :: C_INFTY      = 1._WP
        REAL(WP)    :: RHO_INFTY    = 1._WP
        REAL(WP)    :: T_INFTY	    = 1._WP
        REAL(WP)    :: MU_INFTY	    = 1._WP
        REAL(WP)    :: CP_INFTY	    = 1._WP
        REAL(WP)    :: KAPPA_INFTY  = 1._WP
        REAL(WP)    :: L_INFTY	    = 1._WP
        REAL(WP)    :: P_INFTY      = 1._WP
        REAL(WP)    :: E_INFTY      = 1._WP
    END TYPE    NONDIMENSIONALNUMBER
    
END MODULE MODULE_NONDIMENSIONALNUMBER    