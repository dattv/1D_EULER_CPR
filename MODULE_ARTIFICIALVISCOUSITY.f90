MODULE MODULE_ARTIFICIALVISCOUSITY
    USE MODULE_PRECISION
    USE MODULE_ELEMENT
    
    CONTAINS
    
    SUBROUTINE COMPUTE_ARTIFICIALVISCOUSITYCOEF(SCALE, IVAL, FIRST, LAST, NELE, ELE)
    IMPLICIT NONE 
    INTEGER(IP), INTENT(IN) :: IVAL, FIRST, LAST, NELE
    TYPE(ELEMENT), DIMENSION(1:NELE), INTENT(INOUT), TARGET :: ELE
    TYPE(ELEMENT), DIMENSION(:), POINTER    :: ELE_PTR
    REAL(WP), INTENT(IN)    :: SCALE
    INTEGER(IP) :: IELE, SIZE
    REAL(WP), DIMENSION(1:ELE(1)%ORDER + 1) :: UT, U_HAT, UU_HAT
    REAL(WP)    :: DOT_UU, DOT_UU_HAT
    REAL(WP)    :: S, K, SO
    REAL(WP), DIMENSION(1:ELE(1)%ORDER + 1) :: EPSO, MAX_VAL, MIN_VAL
    REAL(WP)    :: RANGE1, RANGE2, RANGE3
    
    DO IELE = FIRST, LAST, 1
        SIZE = ELE(IELE)%ORDER + 1
        
        UT = MATMUL(ELE(IELE)%COMSYS(1)%INV_V, ELE(IELE)%CONSERVATIVE(:,IVAL))
        UT(SIZE) = 0._WP
        

        U_HAT = MATMUL(ELE(IELE)%COMSYS(1)%V, UT)
        DOT_UU = DOT_PRODUCT(ELE(IELE)%CONSERVATIVE(1:SIZE,IVAL), ELE(IELE)%CONSERVATIVE(1:SIZE,IVAL))
        
        UU_HAT(1:SIZE) = ELE(IELE)%CONSERVATIVE(1:SIZE,IVAL) - U_HAT(1:SIZE)
        DOT_UU_HAT = DOT_PRODUCT(UU_HAT, UU_HAT)
        

        S = LOG10(DOT_UU_HAT/DOT_UU)
        
        IF (.NOT. IEEE_IS_FINITE(S) .OR. ISNAN(S)) S = -6._WP
        
        K = 10.5_WP
        SO = -3._WP*LOG10(REAL(ELE(IELE)%ORDER, WP))
        
        MAX_VAL  = MAXVAL(ABS(ELE(IELE)%PRIMATIVE(:,2)))
        MIN_VAL  = MAXVAL(ABS(ELE(IELE)%PRIMATIVE(:,2))) + (ELE(IELE)%C(:))
        !MIN_VAL  = ABS(MINVAL(ELE(IELE)%PRIMATIVE(:,2)))
        !
        !IF (MAX_VAL > MIN_VAL) THEN 
        !    EPSO = ELE(IELE)%DX/REAL(ELE(IELE)%ORDER, WP)*MAX_VAL/SCALE
        !ELSE
        !    EPSO = ELE(IELE)%DX/REAL(ELE(IELE)%ORDER, WP)*MIN_VAL/SCALE
        !END IF
            !EPSO = (-ELE(IELE)%COMSYS(1)%DX_MAX/2._WP + 1._WP)*ELE(IELE)%DX*MIN_VAL/2000.
            EPSO = ELE(IELE)%COMSYS(1)%DX_MAX*ELE(IELE)%DX*MIN_VAL/SCALE/2._WP
        
        RANGE1 = ABS(S<(SO-K))
        RANGE2 = ABS((SO-K)<S .AND. S<(SO+K))
        RANGE3 = ABS(S>(SO+K))
        
        ELE(IELE)%EPSILON(:, IVAL) = EPSO(:)*(0._WP*RANGE1 + 0.5_WP*(1._WP+SIN(ELE(IELE)%MATERIALINFO(1)%PI*(S-SO)/(2._WP*K)))*RANGE2 + RANGE3)
            
    END DO
    
    RETURN
    END SUBROUTINE COMPUTE_ARTIFICIALVISCOUSITYCOEF
    
    SUBROUTINE COMPUTE_AUXILIARYCONS(IVAL, FIRST, LAST, NELE, ELE)
    IMPLICIT NONE
    INTEGER(IP), INTENT(IN) :: IVAL, FIRST, LAST, NELE
    TYPE(ELEMENT), DIMENSION(1:NELE), INTENT(INOUT), TARGET :: ELE
    TYPE(ELEMENT), DIMENSION(:), POINTER    :: CURRE, LEFTE, RIGHTE
    INTEGER(IP) :: IELE, N1
    REAL(WP)    :: U_L, U_R, UCOM_L, UCOM_R, F_L, F_R
    REAL(WP)    :: K
    REAL(WP), DIMENSION(ELE(1)%ORDER+1) :: DU
    
    K = 0.5_WP
    DO IELE = FIRST, LAST, 1
        LEFTE => ELE(IELE)%LEFT
        CURRE => ELE(IELE:IELE)
        RIGHTE => ELE(IELE)%RIGHT
        
        N1 = CURRE(1)%ORDER + 1
        
        U_L = LEFTE(1)%CONSERVATIVE(N1,IVAL)
        U_R = CURRE(1)%CONSERVATIVE(1,IVAL)
        UCOM_L = K*U_L + (1._WP - K)*U_R
        F_L = U_R
        
        U_L = CURRE(1)%CONSERVATIVE(N1,IVAL)
        U_R = RIGHTE(1)%CONSERVATIVE(1,IVAL)
        UCOM_R = K*U_L  + (1._WP - K)*U_R
        F_R = U_L
        
        DU(1:N1) = MATMUL(CURRE(1)%COMSYS(1)%DR(1:N1,1:N1), CURRE(1)%CONSERVATIVE(1:N1,IVAL))
        
        
        ! FLUX RECONSTRUCTION
        CURRE(1)%Q(:,IVAL) = CURRE(1)%EPSILON(:,IVAL)*(DU(:) + CURRE(1)%CP_COEF(1)%GLB(:)*(UCOM_L - F_L) + CURRE(1)%CP_COEF(1)%GRB(:)*(UCOM_R - F_R))*CURRE(1)%RX(:)
        !CURRE(1)%Q(:,IVAL) = CURRE(1)%EPSILON(:,IVAL)*(DU(:) )*CURRE(1)%RX(:)
    
    END DO
    
    END SUBROUTINE COMPUTE_AUXILIARYCONS
    
    SUBROUTINE COMPUTE_ARTIFICIALVISCOUSITY(IVAL, FIRST, LAST, NELE, ELE)
    IMPLICIT NONE
    INTEGER(IP), INTENT(IN) :: IVAL, FIRST, LAST, NELE
    TYPE(ELEMENT), DIMENSION(1:NELE), INTENT(INOUT), TARGET :: ELE
    TYPE(ELEMENT), DIMENSION(:), POINTER    :: LEFTE, CURRE, RIGHTE
    INTEGER(IP) :: IELE
    REAL(WP)    :: U_L, U_R, UCOM_L, UCOM_R, F_L, F_R
    INTEGER(IP) :: SIZE
    REAL(WP)    :: K
    REAL(WP), DIMENSION(ELE(1)%ORDER + 1)   :: DQ
    
    K = 0.5_WP
    DO IELE = FIRST, LAST, 1
        LEFTE => ELE(IELE)%LEFT
        CURRE => ELE(IELE:IELE)
        RIGHTE => ELE(IELE)%RIGHT
        
        SIZE = CURRE(1)%ORDER + 1
        
        U_L = LEFTE(1)%Q(SIZE, IVAL)
        U_R = CURRE(1)%Q(1, IVAL)
        UCOM_L = U_L*K + (1._WP - K)*U_R
        F_L = U_R
        
        U_L = CURRE(1)%Q(SIZE, IVAL)
        U_R = RIGHTE(1)%Q(1, IVAL)
        UCOM_R = U_L*K + (1._WP - K)*U_R
        F_R = U_L
        
        DQ = MATMUL(CURRE(1)%COMSYS(1)%DR, CURRE(1)%Q(:,IVAL))
        
        ! FLUX RECONSTRUCTION
        CURRE(1)%U_XX(:,IVAL) = (DQ(:) + CURRE(1)%CP_COEF(1)%GLB(:)*(UCOM_L - F_L) + CURRE(1)%CP_COEF(1)%GRB(:)*(UCOM_R - F_R))*CURRE(1)%RX(:)

    END DO
    
    
    END SUBROUTINE COMPUTE_ARTIFICIALVISCOUSITY
END MODULE MODULE_ARTIFICIALVISCOUSITY    