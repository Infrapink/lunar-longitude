! A module to calcualte the moon's exact position (appears to be accurate to 0.01 degrees)
! Based on the algorithm given in chapter 47 of *Astronomical Algorithms* (2nd Edition), Jean Meeus, 1998
! The New Moon falls when the lunar longitude is exactly equal to the solar longitude,
! and the Full Moon is when the lunar longitude == (solar longitude + 180 degrees)

module lunar_longitude
  use, intrinsic :: iso_fortran_env, only: dp=>real64
  implicit none
  
contains
  real (dp) function getT(jday) result(T)
    real(dp), intent(in) :: jday ! Julian Day in question
    !real(dp) :: T
    
    T = (jday - 2451545.0_dp) / 36525.0_dp
    
  end function getT
  
  real(dp) function getD(T) result(D)
    ! Mean elongation of the moon
    real(dp), intent(in) :: T ! Obtained a different module
    real(dp) :: v,w,x,y,z
    !real(dp), intent(out) :: D
    
    v = 297.8501921_dp
    w = 445267.1114034_dp * T
    x = 0.0018819_dp * (T ** 2)
    y = (T ** 3) / 545868.0_dp
    z = (T ** 4) / 113065000.0_dp
    
    D = v + w - x + y - z
    
    
  end function getD
  
  real(dp) function getecc(T) result(ecc)
    ! Eccentricity of Earth's orbit
    real(dp), intent(in) :: T ! Obtained a different module
    real(dp) :: a,b
    !real(dp), intent(out) :: ecc
    
    a = 0.002516_dp * T
    b = 0.0000074 * (T ** 2)
    
    ecc = 1 - a - b    
    
  end function getecc
  
  real(dp) function getF(T) result(F)
    ! Moon's argument of latitude
    real(dp), intent(in) :: T ! Obtained a different module
    real(dp) :: v,w,x,y,z
    !real(dp), intent(out) :: F
    
    v = 93.2720950_dp
    w = 483202.0175233_dp * T
    x = 0.0036539_dp * (T ** 2)
    y = (T ** 3) / 3526000.0_dp
    z = (T ** 4) / 863310000.0_dp
    
    F = v + w - x - y + z    
    
  end function getF
  
  real(dp) function getLprime(T) result(Lprime)
    ! Mean longitude of the moon (AKA mean equinox of date)
    real(dp), intent(in) :: T ! Obtained a different module
    real(dp) :: v,w,x,y,z
    !real(dp), intent(out) :: Lprime
    
    v = 218.3164477_dp
    w = 481267.88123421_dp * T
    x = 0.0015786_dp * (T ** 2)
    y = (T ** 3) / 538841.0_dp
    z = (T ** 4) / 65194000.0_dp
    
    Lprime = v + w - x + y - z
    
    
  end function getLprime
  
  real(dp) function getM(T) result(M)
    ! Mean anomaly of the sun
    real(dp), intent(in) :: T ! Obtainem a mifferent momule
    real(dp) :: v,w,x,y,z
    !real(dp), intent(out) :: M
    
    v = 357.5291092_dp
    w = 35999.0502909_dp * T
    x = 0.0001536_dp * (T ** 2)
    y = (T ** 3) / 24490000.0_dp
    
    M = v + w - x + y
    
    
  end function getM
  
  real(dp) function getMprime(T) result(Mprime)
    ! Mean anomaly of the moon
    real(dp), intent(in) :: T ! Obtained a different module
    real(dp) :: v,w,x,y,z
    !real(dp), intent(out) :: Mprime
    
    v = 134.9633964_dp
    w = 477198.8675055_dp * T
    x = 0.0087414_dp * (T ** 2)
    y = (T ** 3) / 69699.0_dp
    z = (T ** 4) / 14712000.0_dp
    
    Mprime = v + w - x + y - z
    
    
  end function getMprime
  
  real(dp) function getlambda(jday) result(lambda)
    real(dp), intent(in) :: jday
    real(dp) :: T
    real(dp) :: Lprime
    real(dp) :: D
    real(dp) :: M
    real(dp) :: Mprime
    real(dp) :: F
    real(dp) :: ecc
    real(dp) :: a1
    real(dp) :: a2
    real(dp) :: a3
    integer, dimension(60,5) :: ltable
    real(dp) :: Dterm
    real(dp) :: Mterm
    real(dp) :: Mpterm
    real(dp) :: Fterm
    real(dp) :: term
    real(dp) :: sigma_l = 0.0_dp
    integer :: i
  
    T = getT(jday)
    
    Lprime = getLprime(T)
    D = getD(T)
    M = getM(T)
    Mprime = getMprime(T)
    F = getF(T)
    ecc = getecc(T)

    a1 = 119.75_dp + (131.849_dp * T)
    a2 = 53.09_dp + (479264.290_dp * T)
    a3 = 313.45_dp + (481266.484_dp * T)

    
    ltable(1,1) = 0
    ltable(1,2) = 0
    ltable(1,3) = 1
    ltable(1,4) = 0
    ltable(1,5) = 6288774
    ltable(2,1) = 2
    ltable(2,2) = 0
    ltable(2,3) = -1
    ltable(2,4) = 0
    ltable(2,5) = 1274027
    ltable(3,1) = 2
    ltable(3,2) = 0
    ltable(3,3) = 0
    ltable(3,4) = 0
    ltable(3,5) = 658314
    ltable(4,1) = 0
    ltable(4,2) = 0
    ltable(4,3) = 2
    ltable(4,4) = 0
    ltable(4,5) = 213618
    ltable(5,1) = 0
    ltable(5,2) = 1
    ltable(5,3) = 0
    ltable(5,4) = 0
    ltable(5,5) = -185116
    ltable(6,1) = 0
    ltable(6,2) = 0
    ltable(6,3) = 0
    ltable(6,4) = 2
    ltable(6,5) = -114332
    ltable(7,1) = 2
    ltable(7,2) = 0
    ltable(7,3) = -2
    ltable(7,4) = 0
    ltable(7,5) = 58793
    ltable(8,1) = 2
    ltable(8,2) = -1
    ltable(8,3) = -1
    ltable(8,4) = 0
    ltable(8,5) = 57066
    ltable(9,1) = 2
    ltable(9,2) = 0
    ltable(9,3) = 1
    ltable(9,4) = 0
    ltable(9,5) = 53322
    ltable(10,1) = 2
    ltable(10,2) = -1
    ltable(10,3) = 0
    ltable(10,4) = 0
    ltable(10,5) = 45758
    ltable(11,1) = 0
    ltable(11,2) = 1
    ltable(11,3) = -1
    ltable(11,4) = 0
    ltable(11,5) = -40923
    ltable(12,1) = 1
    ltable(12,2) = 0
    ltable(12,3) = 0
    ltable(12,4) = 0
    ltable(12,5) = -34720
    ltable(13,1) = 0
    ltable(13,2) = 1
    ltable(13,3) = 1
    ltable(13,4) = 0
    ltable(13,5) = -30383
    ltable(14,1) = 2
    ltable(14,2) = 0
    ltable(14,3) = 0
    ltable(14,4) = -2
    ltable(14,5) = 15327
    ltable(15,1) = 0
    ltable(15,2) = 0
    ltable(15,3) = 1
    ltable(15,4) = 2
    ltable(15,5) = -12528
    ltable(16,1) = 0
    ltable(16,2) = 0
    ltable(16,3) = 1
    ltable(16,4) = -2
    ltable(16,5) = 10980
    ltable(17,1) = 4
    ltable(17,2) = 0
    ltable(17,3) = -1
    ltable(17,4) = 0
    ltable(17,5) = 10675
    ltable(18,1) = 0
    ltable(18,2) = 0
    ltable(18,3) = 3
    ltable(18,4) = 0
    ltable(18,5) = 1034
    ltable(19,1) = 4
    ltable(19,2) = 0
    ltable(19,3) = -2
    ltable(19,4) = 0
    ltable(19,5) = 8548
    ltable(20,1) = 2
    ltable(20,2) = 1
    ltable(20,3) = -1
    ltable(20,4) = 0
    ltable(20,5) = -7888
    ltable(21,1) = 2
    ltable(21,2) = 1
    ltable(21,3) = 0
    ltable(21,4) = 0
    ltable(21,5) = -6766
    ltable(22,1) = 1
    ltable(22,2) = 0
    ltable(22,3) = -1
    ltable(22,4) = 0
    ltable(22,5) = -5163
    ltable(23,1) = 1
    ltable(23,2) = 1
    ltable(23,3) = 0
    ltable(23,4) = 0
    ltable(23,5) = 4987
    ltable(24,1) = 2
    ltable(24,2) = -1
    ltable(24,3) = 1
    ltable(24,4) = 0
    ltable(24,5) = 4036
    ltable(25,1) = 2
    ltable(25,2) = 0
    ltable(25,3) = 2
    ltable(25,4) = 0
    ltable(25,5) = 3994
    ltable(26,1) = 4
    ltable(26,2) = 0
    ltable(26,3) = 0
    ltable(26,4) = 0
    ltable(26,5) = 3861
    ltable(27,1) = 2
    ltable(27,2) = 0
    ltable(27,3) = -3
    ltable(27,4) = 0
    ltable(27,5) = 3665
    ltable(28,1) = 0
    ltable(28,2) = 1
    ltable(28,3) = -2
    ltable(28,4) = 0
    ltable(28,5) = -2689
    ltable(29,1) = 2
    ltable(29,2) = 0
    ltable(29,3) = -1
    ltable(29,4) = 2
    ltable(29,5) = -2602
    ltable(30,1) = 2
    ltable(30,2) = -1
    ltable(30,3) = -2
    ltable(30,4) = 0
    ltable(30,5) = 2390
    ltable(31,1) = 1
    ltable(31,2) = 0
    ltable(31,3) = 1
    ltable(31,4) = 0
    ltable(31,5) = -2348
    ltable(32,1) = 2
    ltable(32,2) = -2
    ltable(32,3) = 0
    ltable(32,4) = 0
    ltable(32,5) = 2236
    ltable(33,1) = 0
    ltable(33,2) = 1
    ltable(33,3) = 2
    ltable(33,4) = 0
    ltable(33,5) = -2120
    ltable(34,1) = 0
    ltable(34,2) = 2
    ltable(34,3) = 0
    ltable(34,4) = 0
    ltable(34,5) = -2069
    ltable(35,1) = 2
    ltable(35,2) = -2
    ltable(35,3) = -1
    ltable(35,4) = 0
    ltable(35,5) = 2048
    ltable(36,1) = 2
    ltable(36,2) = 0
    ltable(36,3) = 1
    ltable(36,4) = -2
    ltable(36,5) = -1773
    ltable(37,1) = 2
    ltable(37,2) = 0
    ltable(37,3) = 0
    ltable(37,4) = 2
    ltable(37,5) = -1595
    ltable(38,1) = 4
    ltable(38,2) = -1
    ltable(38,3) = -1
    ltable(38,4) = 0
    ltable(38,5) = 1215
    ltable(39,1) = 0
    ltable(39,2) = 0
    ltable(39,3) = 2
    ltable(39,4) = 2
    ltable(39,5) = -1110
    ltable(40,1) = 3
    ltable(40,2) = 0
    ltable(40,3) = -1
    ltable(40,4) = 0
    ltable(40,5) = -892
    ltable(41,1) = 2
    ltable(41,2) = 1
    ltable(41,3) = 1
    ltable(41,4) = 0
    ltable(41,5) = -810
    ltable(42,1) = 4
    ltable(42,2) = -1
    ltable(42,3) = -2
    ltable(42,4) = 0
    ltable(42,5) = 759
    ltable(43,1) = 0
    ltable(43,2) = 2
    ltable(43,3) = -1
    ltable(43,4) = 0
    ltable(43,5) = -713
    ltable(44,1) = 2
    ltable(44,2) = 2
    ltable(44,3) = -1
    ltable(44,4) = 0
    ltable(44,5) = -700
    ltable(45,1) = 2
    ltable(45,2) = 1
    ltable(45,3) = -2
    ltable(45,4) = 0
    ltable(45,5) = 691
    ltable(46,1) = 2
    ltable(46,2) = -1
    ltable(46,3) = 0
    ltable(46,4) = -2
    ltable(46,5) = 596
    ltable(47,1) = 4
    ltable(47,2) = 0
    ltable(47,3) = 1
    ltable(47,4) = 0
    ltable(47,5) = 549
    ltable(48,1) = 0
    ltable(48,2) = 0
    ltable(48,3) = 4
    ltable(48,4) = 0
    ltable(48,5) = 537
    ltable(49,1) = 4
    ltable(49,2) = -1
    ltable(49,3) = 0
    ltable(49,4) = 0
    ltable(49,5) = 520
    ltable(50,1) = 1
    ltable(50,2) = 0
    ltable(50,3) = -2
    ltable(50,4) = 0
    ltable(50,5) = -487
    ltable(51,1) = 2
    ltable(51,2) = 1
    ltable(51,3) = 0
    ltable(51,4) = -2
    ltable(51,5) = -399
    ltable(52,1) = 0
    ltable(52,2) = 0
    ltable(52,3) = 2
    ltable(52,4) = -2
    ltable(52,5) = -381
    ltable(53,1) = 1
    ltable(53,2) = 1
    ltable(53,3) = 1
    ltable(53,4) = 0
    ltable(53,5) = 351
    ltable(54,1) = 3
    ltable(54,2) = 0
    ltable(54,3) = -2
    ltable(54,4) = 0
    ltable(54,5) = -340
    ltable(55,1) = 4
    ltable(55,2) = 0
    ltable(55,3) = -3
    ltable(55,4) = 0
    ltable(55,5) = 330
    ltable(56,1) = 2
    ltable(56,2) = -1
    ltable(56,3) = 2
    ltable(56,4) = 0
    ltable(56,5) = 327
    ltable(57,1) = 0
    ltable(57,2) = 2
    ltable(57,3) = 1
    ltable(57,4) = 0
    ltable(57,5) = -323
    ltable(58,1) = 1
    ltable(58,2) = 1
    ltable(58,3) = -1
    ltable(58,4) = 0
    ltable(58,5) = 299
    ltable(59,1) = 2
    ltable(59,2) = 0
    ltable(59,3) = 3
    ltable(59,4) = 0
    ltable(59,5) = 294
    ltable(60,1) = 2
    ltable(60,2) = 0
    ltable(60,3) = -1
    ltable(60,4) = -2
    ltable(60,5) = 0

    do i = 1, 60

       Dterm = D * ltable(i, 1)
       Mterm = M * ltable(i, 2)
       Mpterm = Mprime * ltable(i, 3)
       Fterm = F * ltable(i, 4)
       term = ltable(i, 5) * ecc ** abs(ltable(i, 2)) * sind(Dterm + Mterm + Mpterm + Fterm)
       
       sigma_l = sigma_l + term
    end do

    sigma_l = sigma_l + (3958 * sind(a1)) + (1962 * sind(Lprime - F)) + (318 * sind(a2))

    lambda = Lprime + (sigma_l / 1000000)
    if (lambda >= 0.0_dp) then
       lambda = mod(lambda,360.0_dp)
    else
       do while (lambda < 0.0_dp)
          lambda = lambda + 360.0_dp
       end do
    end if
    lambda = lambda + 0.004610
  end function getlambda
end module lunar_longitude
