MODULE MODULE_RIEMANNFLUX
    USE MODULE_PRECISION
    
    CONTAINS
    
    SUBROUTINE RUSANOV(GAMMA_L, GAMMA_R, RHO_L, RHO_R, U_L, U_R, RHOE_L, RHOE_R, RHOU_L, RHOU_R, P_L, P_R, F1, F2, F3)
    REAL(WP), INTENT(IN)    :: GAMMA_L, GAMMA_R, RHO_L, RHO_R, U_L, U_R, RHOE_L, RHOE_R, RHOU_L, RHOU_R, P_L, P_R
    REAL(WP), INTENT(OUT)   :: F1, F2, F3
    REAL(WP)    :: AMID, UMID
    
    AMID = SQRT(0.5_WP*(P_L + P_R)*(GAMMA_L + GAMMA_R)/(RHO_L + RHO_R))
    UMID = 0.5_WP*ABS(U_L + U_R)
    
    F1 = 0.5_WP*(RHOU_L + RHOU_R                             - (AMID + UMID)*(RHO_R - RHO_L))
    F2 = 0.5_WP*(RHO_L*U_L*U_L + P_L + RHO_R*U_R*U_R + P_R   - (AMID + UMID)*(RHOU_R - RHOU_L))
    F3 = 0.5_WP*(U_L*(RHOE_L + P_L) + U_R*(RHOE_R + P_R)     - (AMID + UMID)*(RHOE_R - RHOE_L))
    END SUBROUTINE RUSANOV
END MODULE MODULE_RIEMANNFLUX    