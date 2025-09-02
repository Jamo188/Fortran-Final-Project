program Final_Project_Moore
implicit none

! code by Jamison Moore

!        ---------------------------
!    ---This is the declaration section---
!        ---------------------------

real, Dimension(5,72) :: PRES_hPa, TEMP, RELH, HGHT, DGZ   !variables for the data that is being read in. 
! ^ Here, I am using a two dimensional array for the data being read in. The first dimension, 5, represents the unit number of each input file. The second dimension, 72, is the number of rows in the input files the program is reading in.

real, Dimension(5) :: Lrate0to1km, Lrate0to3km, Lrate0to6km                  !variables for the lapse rates
real, Dimension(5) :: HGHTat1000mb, height1000to700mb, height1000to500mb     !variables for the height at 1000 mb and partial thickness
real, Dimension(5) :: AvgRELH, AvgRELHinDGZ, AvgRELHdiff                     !variables for average relative humidity over the sounding and over the DGZ and their difference

integer,  Dimension(5) :: PRES_700, PRES_500, HGHT_1km, HGHT_3km, HGHT_6km, DGZlow, DGZhigh  !variables used to store where various heights and pressures are located
character(len=15), Dimension(5) :: Date        !variabe for the date

real :: SDLrate0to1km, SDLrate0to3km, SDLrate0to6km, SDheight1000to700mb, SDheight1000to500mb  !variables for the Standard Deviations of various measured parameters.

real :: average   !the average function

integer :: p, q   !These integers are used to make sure that the if statements used to find the various heights and pressures stop running after the hight or pressure is found.
integer :: l      !This is used to count the number of layers in the sounding that are in the DGZ, used to get the average relative humidity in the DGZ
integer :: i, j   !Index values for the do loops

!        ---------------------------
!    ---This is the execution section---
!        ---------------------------

! The program opens the five input files and designates the files as input files that the program will read from. 
open(unit=1,file='input_Dec2007.txt',status='unknown',action='read')
open(unit=2,file='input_Jan2008.txt',status='unknown',action='read')
open(unit=3,file='input_Feb2010.txt',status='unknown',action='read')
open(unit=4,file='input_Jan2011.txt',status='unknown',action='read')
open(unit=5,file='input_Mar2017.txt',status='unknown',action='read')

! The program opens or creates a file named 'output_final_project_Moore.txt' and designates the file as an output file that the program will write to.
open(unit=10,file='output_final_project_Moore.txt',status='unknown',action='write')

! Start of main program.
do i = 1,5   ! do loop for each of the five input files. The index i equals the unit number of the file being operated on. (i = file unit number)  
  p = 0
  q = 0   ! sets the counting variables to zero.
  l = 0
  read(i,100) Date(i) ! reads in the date of the sounding from the file. The index of this first do loop matches the unit numbers of the five files. 
  100 format (T37, A15, ////)
  read(i,200) HGHTat1000mb(i) ! reads in the height at 1000mb, this is always on the first line of data on the input files
  200 format (9x, f6.0)
  
  do j = 1,72   ! Here is the second do loop nested within the first. The index j represents the row number of the data of file i.
  
    read(i,300) PRES_hPa(i, j),   HGHT(i, j),   TEMP(i, j),   RELH(i, j) ! reads in the data for our calculations: Pressure, Height, Temperature, and Relative Humidity.
    300 format (2x, f5.1, 2x, f6.0, 1x, f5.1, 12x, f3.0)
    
    ! ^ Here is where each value in the two dimensional array is defined. The i represents the file unit number and the j represents the row in that file. 
    ! For example, HGHT(3, 44) would return the height value that is 44 rows down on the third input file. 
    
    ! This if statement is used to find the row value j for both the 700 and 500 mb levels of the 5 soundings. Since the five soundings have measurements at exactly 700 and 500 mb, the if statement is simple.
    if (PRES_hPa(i,j) == 700) then
      PRES_700(i) = j
    elseif (PRES_hPa(i,j) == 500) then
      PRES_500(i) = j
    end if    
    
    ! These next three if statements are similar to the previous one. They are used to find the row value j for 1km 3km and 6km above the surface. Since these exact heights are not on the soundings, the closest height must be used.
    ! The elevation of the location of the soundings was 361m so the values closest to 1361m, 3316m, and 6361m are found.
    if ((HGHT(i,j) > 1361).and.(p == 0)) then                     ! Once the height value read in is higher than 1361 then...
      if ((HGHT(i,j) - 1361) < (ABS(HGHT(i,j-1) - 1361))) then    ! ...the program finds whether that value or the value previous is closer and then saves the appropriate row j value
        HGHT_1km(i) = j
      else
        HGHT_1km(i) = j - 1
      end if
      p = 1   !The p values in these if statements ensure that after the initial if statement is true once then the if statement no longer works until the next input file is started.
    end if
    
    if ((HGHT(i,j) > 3361).and.(p == 1)) then                     ! separate if statements for 3km and 6km
      if ((HGHT(i,j) - 3361) < (ABS(HGHT(i,j-1) - 3361))) then
        HGHT_3km(i) = j
      else
        HGHT_3km(i) = j - 1
      end if
      p = 2
    end if
    
    if ((HGHT(i,j) > 6361).and.(p == 2)) then
      if ((HGHT(i,j) - 6361) < (ABS(HGHT(i,j-1) - 6361))) then
        HGHT_6km(i) = j
      else
        HGHT_6km(i) = j - 1
      end if
      p = 3
    end if
    
    ! Program finds the upper and lower end of the DGZ. 

    if ((TEMP(i,j) <= -10).and.(TEMP(i,j) >= -20)) then   ! If the TEMP value is between -10 and -20 then...
      DGZ(i,j) = RELH(i,j)                                ! ...record the DGZ 
      l = l + 1
      if (q == 0) then
        DGZlow(i) = j          ! The program records the row number j of the low end of the DGZ. only if it is the first time in the file
        q = 1                  ! q, just like p, is there to ensure that the DGZlow value is only defined once and not replaced if the first if statement is true again.
      end if
    elseif (q == 1) then
      DGZhigh(i) = j
      DGZ(i,j) = 0
      q = 2
    else
      DGZ(i,j) = 0
    end if


  end do
  
  ! 1000 to 700mb and 1000 to 500mb heights, Height at 700/500mb minus the height at 1000mb.

  height1000to700mb(i) = HGHT(i, PRES_700(i)) - HGHTat1000mb(i)
  height1000to500mb(i) = HGHT(i, PRES_500(i)) - HGHTat1000mb(i)
  
  !Lapse Rates: change in temperature over change in height
  
  Lrate0to1km(i) = (-(TEMP(i, HGHT_1km(i))-TEMP(i, 1)))/((HGHT(i, HGHT_1km(i))-HGHT(i, 1))/1000)
  Lrate0to3km(i) = (-(TEMP(i, HGHT_3km(i))-TEMP(i, 1)))/((HGHT(i, HGHT_3km(i))-HGHT(i, 1))/1000)
  Lrate0to6km(i) = (-(TEMP(i, HGHT_6km(i))-TEMP(i, 1)))/((HGHT(i, HGHT_6km(i))-HGHT(i, 1))/1000)
  
  !Average Relative Humidity in the DGZ vs. the 1000-500mb level of sounding
  
  AvgRELH(i) = average(RELH(i,1:PRES_500(i)), PRES_500(i), PRES_500(i))
  AvgRELHinDGZ(i) = average(DGZ(i,1:PRES_500(i)), PRES_500(i), l)
  AvgRELHdiff(i) = AvgRELHinDGZ(i) - AvgRELH(i)

end do

! use the call statement to trigger the code for standard deviation. 
call standardDeviation (Lrate0to1km, 5, SDLrate0to1km)
call standardDeviation (Lrate0to3km, 5, SDLrate0to3km)
call standardDeviation (Lrate0to6km, 5, SDLrate0to6km)

call standardDeviation (height1000to700mb, 5, SDheight1000to700mb)
call standardDeviation (height1000to500mb, 5, SDheight1000to500mb)

! Write the results out on to the output file:

write(10,*) 
write(10,*)
write(10,1000) Date(1), Date(2), Date(3), Date(4), Date(5)
1000 format (T32, 5(A15, 8x))
write(10,1100) HGHT(1,DGZlow(1)), ' - ', HGHT(1,DGZhigh(1)),  HGHT(2,DGZlow(2)), ' - ', HGHT(2,DGZhigh(2)), HGHT(3,DGZlow(3)), &
 ' - ', HGHT(3,DGZhigh(3)),  HGHT(4,DGZlow(4)), ' - ', HGHT(4,DGZhigh(4)),  HGHT(5,DGZlow(5)), ' - ', HGHT(5,DGZhigh(5))
1100 format ('Dendritic Growth Zone (km):', T34, 5(F5.0, A3, F5.0, 10x) )
write(10,*)
write(10,1200) height1000to700mb(1), height1000to700mb(2), height1000to700mb(3), height1000to700mb(4), height1000to700mb(5)
1200 format ('1000 to 700mb Thickness (km):', T38, 5(F5.0, 18x) )
write(10,1210) height1000to500mb(1), height1000to500mb(2), height1000to500mb(3), height1000to500mb(4), height1000to500mb(5)
1210 format ('1000 to 500mb Thickness (km):', T38, 5(F5.0, 18x) )
write(10,*)
write(10,1300) Lrate0to1km(1), Lrate0to1km(2), Lrate0to1km(3), Lrate0to1km(4), Lrate0to1km(5)
1300 format ('0 to 1 km lapse rate (°C/km):', T37, 5(F5.2, 18x) )
write(10,1310) Lrate0to3km(1), Lrate0to3km(2), Lrate0to3km(3), Lrate0to3km(4), Lrate0to3km(5)
1310 format ('0 to 3 km lapse rate (°C/km):', T37, 5(F5.2, 18x) )
write(10,1320) Lrate0to6km(1), Lrate0to6km(2), Lrate0to6km(3), Lrate0to6km(4), Lrate0to6km(5)
1320 format ('0 to 6 km lapse rate (°C/km):', T37, 5(F5.2, 18x) )
write(10,*)
write(10,1410) AvgRELHinDGZ(1), AvgRELHinDGZ(2), AvgRELHinDGZ(3), AvgRELHinDGZ(4), AvgRELHinDGZ(5)
1410 format ('Average % humidity over DGZ:' T37, 5(F5.2, 18x) )
write(10,1420) AvgRELH(1), AvgRELH(2), AvgRELH(3), AvgRELH(4), AvgRELH(5)
1420 format ('Average % humidity over sounding:' T37, 5(F5.2, 18x) )
write(10,1430) AvgRELHdiff(1), AvgRELHdiff(2), AvgRELHdiff(3), AvgRELHdiff(4), AvgRELHdiff(5)
1430 format ('Humidity difference:' T37, 5(F5.2, 18x) )
write(10,*)
write(10,*) 'Standard Deviations:' 
write(10,1510) '0 to 1 km lapse rates:', SDLrate0to1km
write(10,1510) '0 to 3 km lapse rates:', SDLrate0to3km
write(10,1510) '0 to 6 km lapse rates:', SDLrate0to6km
1510 format (A22, 7x, f5.2)
write(10,1520) '1000 to 700mb Thicknesses:', SDheight1000to700mb
write(10,1520) '1000 to 500mb Thicknesses:', SDheight1000to500mb
1520 format (A26, 3x, f7.2)

!        ---------------------------
!    ---This is the termination section---
!        ---------------------------

! don't forget to close the files
close(unit=1)
close(unit=2)
close(unit=3)
close(unit=4)
close(unit=5)
close(unit=10)

end program

!-----------------------------------------------

subroutine standardDeviation (array, n, SD)  ! Subroutine for Standard Deviation, Reads in the array and the total number of values in the array. Returns the Standard deviation of the vlues in the array.
implicit none

real, dimension(n), intent(in) :: array
integer, intent(in) :: n
real, intent(out) :: SD
real :: average, AVG, SDsum
integer :: i

SDsum = 0.0

AVG = average(array, n, n)      ! Average function 

do i = 1, n
  SDsum = SDsum + (ABS(array(i)-AVG))**2     ! Standard deviation is found by taking the sum of each value in the data subtracted by the average and squared.
end do

SD = (SDsum/n)**1/2                     ! Then the square root of the sum, divided by the number of data points in the set.

end subroutine standardDeviation

!-----------------------------------------------

function average (array, n, qty)  ! function for average.
implicit none

real, dimension(n), intent(in) :: array
integer, intent(in) :: n, qty   ! n and qty are both variables for the total number of values in the data set. They are both necessary because of the Relative humidity in the DGZ. The array DGZ has 72 values in it but many of them are zero. To exclude these zeros I gave the program the total number of values in the array (n) for the do loop as well as the total number of nonzero values in the array (qty) to divide the total by.
real :: total
real :: average
integer :: i

total = 0.0

do i= 1, n
  total = total + array(i)   ! find the total of the value in the array
end do

average = total/qty          ! the average is the sum over the quantity of the array

end function average