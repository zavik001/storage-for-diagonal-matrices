      INTEGER FUNCTION check(n, m)
      IMPLICIT NONE 
      INTEGER n
      INTEGER m
      check = 0
      IF((n.ge.2982).or.(n.lt.4)) THEN
         check = 1
      END IF
      IF((m.ge.n-4).or.(m.lt.0)) THEN
         check = 1
      END IF
      RETURN
      END FUNCTION check


      REAL FUNCTION randd(seed)
      IMPLICIT NONE
      INTEGER seed
      seed = MOD(seed * 110315245 + 12345, 214783648)
      randd =REAL(seed) / 32174641.0
      RETURN
      END FUNCTION randd
      

      PROGRAM GENERATOR
      IMPLICIT NONE 
      INTEGER type
      INTEGER n
      INTEGER m
      INTEGER check

      WRITE(*, *) 'print type: 1-binary 2-txt:'
      READ(*, *) type
      WRITE(*, *) 'size of matrix:'
      READ(*, *) n
      WRITE(*, *) 'size between diagonals:'
      READ(*, *) m

      IF (check(n, m).eq.1) THEN
         WRITE(*, *) 'incorrect data: n, m'
         GO TO 100
      END IF
      IF ((type.ge.3).or.(type.lt.1)) THEN
         WRITE(*, *) 'incorrect data: type'
         GO TO 100
      END IF

      IF (type.eq.2) THEN 
         call generateTxt(n, m)
      ELSE 
         call generateBinary(n,m)
      END if

      GO TO 200

100   WRITE(*, *) 'ERROR'
200   END


      SUBROUTINE generateTxt(n, m)
      implicit none
      REAL randd
      INTEGER n
      INTEGER m
      INTEGER i, j
      INTEGER seed
      INTEGER offsets(9)
      seed = 1007*m*n

      open(1, file='txtFiles/vector.txt', action='WRITE')
      DO i = 1, n 
         write(1, '(F20.10)') randd(seed)
      END DO
      close(1)

      open(2, file='txtFiles/matrix.txt', action='WRITE')
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         DO i = 1, n + offsets(j)
            write(2, '(F20.10, 1X)', advance='no') randd(seed)
         END DO
         write(2, *)
      END DO
      close(2)

      open(3, file='txtFiles/input.txt', action='WRITE')
      write(3, '(I20, 1X)', advance='no') n
      write(3, '(I20)', advance='no') m
      close(3)
      END


      SUBROUTINE generateBinary(n, m)
      implicit none
      REAL randd
      INTEGER n
      INTEGER m
      INTEGER i, j
      INTEGER seed
      INTEGER offsets(9)
      REAL value
      seed = 9846*m*n

      open(1, file='BinaryFiles/vector.dat', access='DIRECT', recl=8)
      DO i = 1, n
         value = randd(seed)
         write(1, rec=i) value
      END DO
      close(1)

      open(2, file='BinaryFiles/matrix.dat', access='DIRECT', recl=8)
      offsets = (/ -m-4, -m-3, -m-2, -1, 0, -1, -m-2, -m-3, -m-4 /)
      DO j = 1, 9
         DO i = 1, n + offsets(j)
            value = randd(seed)
            write(2, rec=(j-1)*(n+1) + i) value
         END DO
      END DO
      close(2)

      open(3, file='BinaryFiles/input.dat', access='DIRECT', recl=8)
      write(3, rec=1) n
      write(3, rec=2) m
      close(3)
      END